from ete2 import NCBITaxa
from fn import _
import argparse
from itertools import ifilterfalse, starmap, dropwhile, imap as map
from functools import partial
from toolz.dicttoolz import keymap, merge
import sys
from plumbum import local
import csv

ncbi = NCBITaxa() # downloads database and creates SQLite database if needed
def taxonomy(taxid):
    lineage = ncbi.get_lineage(taxid)
    ranks = ncbi.get_rank(lineage)
    names = ncbi.get_taxid_translator(lineage)
    def make_d(lineage, ranks, names):
        for lin in lineage:
            if ranks[lin] == 'no rank':
                continue
            yield (ranks[lin], names[lin])
    return dict(make_d(lineage, ranks, names))

def blast_cmd(cmd, **opts):
    cmd_opts = keymap('-{}'.format, opts).items()
    process = local[cmd][cmd_opts]
    print process
    for line in process.popen().iter_lines(retcode=None):
        yield line[0]

def get_taxid(db, seqid): # (Path, str) -> dict[str,str]
   res = blast_cmd('blastdbcmd', db=db, outfmt="'%g %T'", entry="'%s'" % seqid)
   res = filter(bool, res)
   res = map(lambda s: s.strip("'"), res)
   return dict(map(unicode.split, res))

# m8_fields = "query	subject	identity	aln-len	mismatch	gap-openings	q.start	q.end	s.start	s.end	log(e-value)	bit-score".split()

def dictmap(f, d): return starmap(f, d.items())

def simple_rs(p):
    r = csv.reader(open(p), delimiter='\t')
    rows = dropwhile(lambda x: x[0].startswith('#'), r)
    def to_dict(row):
        return dict(query=row[0], subject=row[1])
    return map(to_dict, rows)

def simple_gsnap(p):
    r = csv.reader(open(p), delimiter='\t')
    rows = dropwhile(lambda x: x[0].startswith('@'), r)
    def to_dict(row):
        return dict(query=row[0], subject=row[2])
    return map(to_dict, rows)

def get_gi(seqid): return seqid.split("|")[1]

def blast2summary_dict(db, path, reader): # (Path, Path) -> list[dict]
  """Reading in a blast output file, lookup all seqids to get taxids with a single blastdbcmd.
  Then, lookup the taxonomy using ETE2 via the taxid, and add that info to the blast info."""
  rows = reader(path)
  seqids = map(_['subject'], rows)
  taxids = get_taxid(db, ','.join(seqids))
  gis = map(get_gi, seqids)
  matches = dict((taxids[gi], row) for gi, row in zip(gis,rows) if gi in taxids)
  return dictmap(lambda tid,row: merge(row, taxonomy(tid)), matches)

def blast2summary(db, path, reader): # (Path,Path,Path) -> None
    with_taxonomies = list(blast2summary_dict(db, path, reader))
    head = with_taxonomies[0]
    writer = csv.DictWriter(sys.stdout, fieldnames=head.keys(), delimiter='\t')
    writer.writeheader() # woops
    for row in with_taxonomies:
      writer.writerow(row)

def togi(__, path, reader):
  rows = reader(path)
  base = 'taxinfo.%s'
  head = rows.next()
  with open(base % 'gi', 'w') as outGI, open(base % 'info.tsv', 'w') as outInfo:
    csvout = csv.DictWriter(outInfo, fieldnames=head.keys(), delimiter='\t')
    csvout.writeheader()
    csvout.writerow(head)
    for row in rows:
      outGI.write( get_gi( row['subject'] ) + '\n')
      csvout.writerow(row)

def join(infoPath, taxidPath): # prints to stdout
  with open(infoPath) as _infos, open(taxidPath) as taxids:
    infos = csv.DictReader(_infos)
    def merge_info(taxid, row): return merge(row, taxonomy(taxid))
    outs = map(merge_info, taxids, infos):
    head = outs.next()
    csvout = csv.DictWriter(sys.stdout, fieldnames=head.keys(), delimiter='\t')
    csvout.writeheader()
    csvout.writerow(head)
    csv.writerows(outs)

if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument("input", help="GSNAP or RapSearch2 (m8) output file")
  parser.add_argument("--db", help="Blast db for blastdbcmd")
  group = parser.add_mutually_exclusive_group(required=True)
  group.add_argument('--gsnap', action='store_true', help="File is sam as produced by gsnap")
  group.add_argument('--rapsearch', action='store_true', help="File is m8 as produced by rapsearch")
  args = parser.parse_args()
  if args.gsnap:
    togi(args.db, args.input, simple_gsnap)
  elif args.rapsearch:
    togi(args.db, args.input, simple_rs)
  else:
    raise ValueError("Must choose either gsnap or rapsearch")

#acc2tax = "ftp.ncbi.nih.gov/pub/taxonomy/accession2taxid/prot.accession2taxid"

