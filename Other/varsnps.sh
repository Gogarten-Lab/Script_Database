#requries bcf tools, samtools, and bowtie2 (or other read mapper)
bowtie2-build ref.fasta refname
#makes bowtie index for reference genome

bowtie2 -x refname -1 read1.fq -2 read2.fq -U unpairedread.fq -S output.sam
#map reads back to reference genome for sam file

samtools view -bS output.sam > output.bam
#convert sam to bam

samtools sort output.bam -O output.sort.bam
#sort by genomic location
#need to do next step or the variant annotations are useless
samtools mpileup --min-BQ number 20-30 \
-f ref.fasta --BCF bamfromprevious.bam | \
 bcftools call --consensus-caller \
--variants-only --pval-threshold 0.05 recommended -Ov -Ob > outputsvariants.bcf ;
#gets the variants from read mapping file


bcftools norm -m-any outputsvariants.bcf | bcftools norm -Ov --check-ref w -f ref.fasta > outputsvariants.vcf ;
#convert to vcf based off of alignment with refernce genome


bcftools view outputsvariants.vcf | vcfutils.pl varFilter -d 18 -w 1 -W 3 -a 1 \
-e 0.05 -p > outputsvariants.filtered.vcf ;
#filters those variants that don't meet a certain threshold
