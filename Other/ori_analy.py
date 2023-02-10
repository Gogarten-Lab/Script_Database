### analyze the origin of replication in archaea (haloarchaea at least)

#####assumes you have: *fna of the genomes (single line), blast file with hits to cdc6 (origin) from the genomes
####usage: ori_analy.py $blastfile $motif[any nt/aa motif]
#output: dict with origins, *.csv file with clusters of motifs

#read in whole genome seqs
all_storage = {}
for fna in glob.glob("*.fna"):
    storage = {}
    contig = ""
    seq = ""
    with open(fna) as ofh:
       
        for line in ofh:
            if not line:
                continue
            line = line.strip()
            if line.startswith(">"):
                contig = line[1:].split(" ")[0]
            else:
                seq = line
            storage[contig] = seq
    all_storage[fna] = storage


#read in ori data from blast file with hits to cdc6
#you need to edit the sstart ssend indexes to match your blast output (and maybe the genome name depending on how you want it displayed (cosmetic))
ori_locs = {}
with open(sys.argv[0]) as ofh:
    
    for line in ofh:
        if not line:
            continue
        line = line.strip()
        genome = line.split("\t")[0].replace(":", "_")
        sstart = line.split("\t")[2]
        send = line.split("\t")[3]
        if int(send) - int(sstart) < 0:
            ori_locs[genome] = send
        if int(send) - int(sstart) > 0:
            ori_locs[genome] = sstart


#this gets the contigs that contain the ori
cdc6_conts = ori_locs.keys()

actually_matters = {}
for genomes, contigs in all_storage.items():
    for name, seq in contigs.items():
        if name in cdc6_conts:
            actually_matters[name] = seq



## this function can find motifs [any nucleotide/AA] of interest 
def motif_calc(sys.argv[1])
    motifs = {}
    for contig, seq in actually_matters.items():
        ctags = [m.start() for m in re.finditer(sys.argv[1], seq)]
        motifs[contig] = ctags


# this will find clusters of your motif within the region of interest
clustered_ctags = {}
for contigs, region in ori_locs.items():
    tmp_list = []
    for inds in motifs[contigs]:
        numba = (int(inds)-int(region))
        if -10000 <= numba <= 10000:
            tmp_list.append(numba)
    clustered_ctags[contigs] = tmp_list


for contig, motifs in clustered_ctags.items():
    print("{} \t {} \t {} \t {}".format(contig, len(motifs), len(ctag_motifs[contig]),
                                     len(actually_matters[contig])),
          file=open("ori_motif_stats.tab", 'a'))