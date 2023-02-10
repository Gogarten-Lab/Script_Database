#!/usr/bin/perl -w
use warnings;
use strict;
use threads;
use threads::shared;

#Subdirectories
mkdir "Outputs";
mkdir "Outputs/AF";
mkdir "Outputs/Distance";
mkdir "Outputs/gANI";
mkdir "Outputs/jANI";
mkdir "Intermediates";
mkdir "Intermediates/splits";
mkdir "Intermediates/BLAST";
mkdir "Intermediates/blastdb";

#globals
my @genomefiles = glob "*.fna *.fasta *.faa *.contig *.fa *.contigs";
my $evalue = "1E-4";
my $task = "";
my ($identity,$coverage,$bootnum);
my (%matrix,%all_lengths,%bootmatrix):shared;

#Parse Input
if (!exists $ARGV[0]){
	die "\nNo inputs detected, please use Default, or type -H for assistance.\n\n";
}
elsif ($ARGV[0] eq "-H"){
	die "\nThe following options are available:\n\n\t-ID:# - The identity cutoff value (0-100).\n\t-CV:# - The coverage cutoff value (0-100).\n\t-BT:# - The number of nonparametric bootstraps for tree building (0-n).\n\t-EVAL:# - Change E-Value cutoff of BLAST search.\n\t-TASK:'task name' If you wish to change which default settings BLAST uses for its search criteria.\n\t-Default - Uses default search criteria as described in Gosselin et al. 2020.\n\nIf you encounter negative values in your matrix, or critical errors send an email to sean.gosselin\@uconn.edu. I will try to answer in a reasonable fassion.\n\n\n";
}
elsif ($ARGV[0] eq "-Default"){
	$identity = .7;
	$coverage = .7;
	$bootnum = 100;
}
else{
	foreach my $Inputs (@ARGV){
		if ($Inputs =~ /\-ID\:(.*)/){
			if ($1 < 1){
				$identity = ($1*100);
			}
			else{
				$identity = $1;
			}
		}
		elsif  ($Inputs =~ /\-CV\:(.*)/){
			if ($1 > 1){
				$coverage = ($1/100);
			}
			else{
				$coverage = $1;
			}
		}
		elsif  ($Inputs =~ /\-BT\:(.*)/){
			$bootnum = $1;
		}
		elsif  ($Inputs =~ /\-EVAL\:(.*)/){
			$evalue = $1;
		}
		elsif  ($Inputs =~ /\-TASK\:(.*)/){
			$task = "-task $1";
		}
	}
}

#Check inputs.
#Could this be more nicely done? Yes. Will I fix this later? Yes.

if($identity){}
else{
	print "\nIdentity value not defined. Use standard (.7)? [Y|N]\n";
	my $idencheck = <STDIN>;
	chomp $idencheck;
	if($idencheck eq "Y"){
		$identity=.7;
	}
	else{
		die "Need ID value to run. Stopping.\n";
	}
}

if($coverage){}
else{
	print "\nCoverage value not defined. Use standard (.7)? [Y|N]\n";
	my $covcheck = <STDIN>;
	chomp $covcheck;
	if($covcheck eq "Y"){
		$coverage=.7;
	}
	else{
		die "Need CV value to run. Stopping.\n";
	}
}

if($bootnum){}
else{
	print "\nBoot value not defined, do you want to use standard (100)? [Y|N]\n";
	my $bootcheck = <STDIN>;
	chomp $bootcheck;
	if($bootcheck eq "Y"){
		$bootnum=100;
	}
	else{
		die "Need bootstrap (BT) count. Stopping.\n";
	}
}

#mainworkflow
if (@genomefiles){
	#splits genomes into fragmented files, and creates blast databases
	#also creates a reference file for lengths. Acts as a checkpoint.
	if(-e "Intermediates/genome_lengths.txt"){
		open (WGL, "< Intermediates/genome_lengths.txt");
		while(<WGL>){
			my @split = split;
			$all_lengths{$split[0]}=$split[1];
		}
		close WGL;
	}
	else{
		threaded("SplitGenomes",\@genomefiles);
		open (WGL, "+> Intermediates/genome_lengths.txt");
		foreach my $genome (sort keys %all_lengths){
			print WGL "$genome\t$all_lengths{$genome}\n";
		}
		close WGL;
	}
	#comprehensive all vs. all BLAST searches and database creation
	threaded("BlastGenome",\@genomefiles);
	#Calculating distance, ANI, AF, and gANI
	threaded("Calculate_ANI",\@genomefiles);
	#outs to file. Sends file type
	Outfile("orig","orig",%matrix);
	#bootstrapping
	for (my $bc=0; $bc<$bootnum; $bc++){
		threaded("Bootstrap_ANI",\@genomefiles);
		Outfile($bc,"matrix",%bootmatrix);
		%bootmatrix=();
	}
}
else{
	print "\nNo genomes present in directory that are recognized.\n\nMake sure to use FASTA formatted files.\n\n";
}

sub threaded{
	my ($sub,$arrayref) = @_;
	my @refdarray = @{$arrayref};
	my @threads;
	foreach my $entry (@refdarray){
		my $thread = threads ->create(\&$sub,$entry);
		push @threads, $thread;
	}
	$_->join() for @threads;
}

sub SplitGenomes {
	#splits genomes into 1020, and formats blastdb's
	open (FILE_TO_SPLIT, "< $_[0]");
	my ($annotation_counter,$wholegenome) = 0;
	my (%printhash);
	my ($annotation,$sequenceinput)="";
	while(<FILE_TO_SPLIT>){
		if ($_=~/\>/){
			if($annotation_counter==0){
				$annotation_counter++;
			}
			else{
				my $contiglength = (length($sequenceinput));
				my @cutcontig =($sequenceinput =~ /(.{1,1020})/g);
				$wholegenome += $contiglength;
				$sequenceinput = "";
				foreach my $cuts (@cutcontig){
					my $cutl=length($cuts);
					next if $cutl < 100;
					my $contigname = "$annotation"."\_$annotation_counter"."\_$contiglength"."\_$cutl";
					$printhash{$contigname}=$cuts;
					$annotation_counter++;
				}
			}
			($annotation) = ($_=~/\>(.*?)[\n\s]/);
			$annotation =~ s/\s//g;
		}
		else{
			chomp $_;
			$sequenceinput .= $_;
		}
	}
	my @cutcontig =($sequenceinput =~ /(.{1,1020})/g);
	my $contiglength = (length($sequenceinput));
	$wholegenome += $contiglength;
	$sequenceinput = "";
	foreach my $cuts (@cutcontig){
		my $cutl=length($cuts);
		next if $cutl < 100;
		my $contigname = "$annotation"."\_$annotation_counter"."\_$contiglength"."\_$cutl";
		$printhash{$contigname}=$cuts;
		$annotation_counter++;
	}
	close FILE_TO_SPLIT;
	$all_lengths{$_[0]}=$wholegenome;
	open (SPLIT, "+> Intermediates/splits/$_[0].split");
	foreach my $contigname (sort keys %printhash){
		print SPLIT ">$contigname\n$printhash{$contigname}\n";
	}
	close SPLIT;
	if (-e "Intermediates/blastdb/$_[0].nhr"){
	}
	else{
		system("makeblastdb -dbtype nucl -in $_[0] -input_type fasta -max_file_sz 2GB -out Intermediates/blastdb/$_[0]");
	}
}

sub BlastGenome {
	#self explanitory
	foreach my $database (@genomefiles){
		if($database eq $_[0]){
			next;
		}
		elsif (-e "Intermediates/BLAST/$_[0]+$database.blast"){
			if(-z "Intermediates/BLAST/$_[0]+$database.blast"){
				system("blastn -db Intermediates/blastdb/$database -query Intermediates/splits/$_[0].split -evalue $evalue -outfmt 6 -out 'Intermediates/BLAST/$_[0]+$database.blast' $task");
			}
			else{
				next;
			}
		}
		else{
			system("blastn -db Intermediates/blastdb/$database -query Intermediates/splits/$_[0].split -evalue $evalue -outfmt 6 -out 'Intermediates/BLAST/$_[0]+$database.blast' $task");
		}
	}
}

#All calculations are done here; name misleading
sub Calculate_ANI{
	#readin query information
	open(SPLIT, "< Intermediates/splits/$_[0].split");
	my(%qsplitsize);
	while(<SPLIT>){
		if($_=~/\>/){
			my($annotation,$splitsize)=($_=~/\>(.*\_.*?\_.*?\_(.*))/);
			$qsplitsize{$annotation}=$splitsize;
		}
		else{}
	}
	close SPLIT;
	foreach my $database (@genomefiles){
		if($database eq $_[0]){
			$matrix{"$_[0]\+$_[0]"} = "1.0\t1.0\t1.0\t0.0";
			next;
		}
		else{}
		#readin database information
		open(DB, "< Intermediates/splits/$database.split");
		my(%dbsplitsize,%dbcontigsize);
		while(<DB>){
			if($_=~/\>/){
				my($annotation,$consize)=($_=~/\>(.*)\_.*?\_(.*?)\_.*/);
				next if exists $dbcontigsize{$annotation};
				$dbcontigsize{$annotation}=$consize;
			}
			else{}
		}
		close DB;
		#start filtering BLAST output
		my(%BestHits,$gANINumerator,$TotalShort,$TotalID,$shortergene);
		my $HitCounter = 0;
		open(BLAST_FILE, "< Intermediates/BLAST/$_[0]+$database.blast");
		while(<BLAST_FILE>){
			my @Blast_Lines = split;
			next if exists $BestHits{$Blast_Lines[0]};
			next if $Blast_Lines[2] < $identity;
			if ($dbcontigsize{$Blast_Lines[1]} <= $qsplitsize{$Blast_Lines[0]}){
				$shortergene = $dbcontigsize{$Blast_Lines[1]};
			}
			else{
				$shortergene = $qsplitsize{$Blast_Lines[0]};
			}
			next if ($shortergene == 0);
			next if (($Blast_Lines[3]/$shortergene) < $coverage);
			my $gANIindv = ($Blast_Lines[3]*($Blast_Lines[2]/100));
			$BestHits{$Blast_Lines[0]} = 1;
			$gANINumerator += $gANIindv;
			$TotalShort += $shortergene;
			$HitCounter ++;
			$TotalID += $Blast_Lines[2];
		}
		close BLAST_FILE;
		if($HitCounter == 0){
			$matrix{"$_[0]\+$database"} = "0.00\t0.00\t0.00\t13.00";
		}
		else{
			my $jANI = ($TotalID/$HitCounter);
			my $gANI = ($gANINumerator/$TotalShort);
			my $AF = ($TotalShort/$all_lengths{$_[0]});
			my $gDistance = -log($gANINumerator/$all_lengths{$_[0]});
			$matrix{"$_[0]\+$database"} = "$jANI\t$gANI\t$AF\t$gDistance";
		}
	}
}

sub Bootstrap_ANI{
	#readin query information
	open(SPLIT, "< Intermediates/splits/$_[0].split");
	my(%qsplitsize,%randomsample,$bootsize);
	my $counter = 0;
	while(<SPLIT>){
		if($_=~/\>/){
			my($annotation,$splitsize)=($_=~/\>(.*\_.*?\_.*?\_(.*))/);
			$qsplitsize{$annotation}=$splitsize;
			$counter++;
		}
		else{}
	}
	close SPLIT;
	my @q_keys = keys %qsplitsize;
	for(my $rngcount=0; $rngcount<$counter; $rngcount++){
		$randomsample{$rngcount} = $q_keys[rand @q_keys];
	}
	foreach my $database (@genomefiles){
		if($database eq $_[0]){
			$bootmatrix{"$_[0]\+$_[0]"} = "1.0\t1.0\t1.0\t0.0";
			next;
		}
		else{}
		#readin database information
		open(DB, "< Intermediates/splits/$database.split");
		my(%dbsplitsize,%dbcontigsize);
		while(<DB>){
			if($_=~/\>/){
				my($annotation,$consize)=($_=~/\>(.*)\_.*?\_(.*?)\_.*/);
				next if exists $dbcontigsize{$annotation};
				$dbcontigsize{$annotation}=$consize;
			}
			else{}
		}
		close DB;
		#start filtering BLAST output
		my(%BestHits,$gANINumerator,$TotalShort,$TotalID,$shortergene);
		my $HitCounter = 0;
		open(BLAST_FILE, "< Intermediates/BLAST/$_[0]+$database.blast");
		while(<BLAST_FILE>){
			my @Blast_Lines = split;
			next if exists $BestHits{$Blast_Lines[0]};
			next if $Blast_Lines[2] < $identity;
			if ($dbcontigsize{$Blast_Lines[1]} <= $qsplitsize{$Blast_Lines[0]}){
				$shortergene = $dbcontigsize{$Blast_Lines[1]};
			}
			else{
				$shortergene = $qsplitsize{$Blast_Lines[0]};
			}
			next if ($shortergene == 0);
			next if (($Blast_Lines[3]/$shortergene) < $coverage);
			my $gANIindv = ($Blast_Lines[3]*($Blast_Lines[2]/100));
			$BestHits{$Blast_Lines[0]} = "$gANIindv\t$Blast_Lines[2]\t$shortergene";
		}
		close BLAST_FILE;
		foreach my $sample (values %randomsample){
			if(exists $BestHits{$sample}){
				my @split = split(/	/,$BestHits{$sample});
				$gANINumerator += $split[0];
				$TotalShort += $split[2];
				$HitCounter++;
				$TotalID += $split[1];
			}
			else{
			}
		}
		if($HitCounter == 0){
			$bootmatrix{"$_[0]\+$database"} = "0.00\t0.00\t0.00\t13.00";
		}
		else{
			my $jANI = ($TotalID/$HitCounter);
			my $gANI = ($gANINumerator/$TotalShort);
			my $AF = ($TotalShort/$all_lengths{$_[0]});
			my $gDistance = -log($gANINumerator/$all_lengths{$_[0]});
			$bootmatrix{"$_[0]\+$database"} = "$jANI\t$gANI\t$AF\t$gDistance";
		}
	}
}

sub Outfile{
	#number or indicator to go after the _ but before file extension.
	my $u1 = shift;
	#file extension
	my $u2 = shift;
	#content of the matrices
	my %toprint = @_;
	#checker tool
	my $currentline = "hi";
	my %handles;
	my @outfiles = ("Outputs/jANI/jANI","Outputs/gANI/gANI","Outputs/AF/AF","Outputs/Distance/Distance");
	foreach my $out1 (@outfiles){
		open (my $fh, "+>", "$out1\_$u1\.$u2");
		print {$fh} join("\t",sort @genomefiles);
		#try a print join approach when you have time
		$handles{$out1}=$fh;
	}
	foreach my $value (sort keys %toprint){
		my ($check) = ($value =~ /(.*?)\+.*/);
		if ($check eq $currentline){
			my $num = 0;
			my @outputs = split(/	/,$toprint{$value});
			foreach my $out (@outfiles){
				print {$handles{$out}} "\t$outputs[$num]";
				$num++;
			}
		}
		else{
			$currentline = $check;
			my $num = 0;
			my @outputs = split(/	/,$toprint{$value});
			foreach my $out (@outfiles){
				print {$handles{$out}} "\n$check\t$outputs[$num]";
				$num++;
			}
		}
	}
	foreach my $out2 (@outfiles){
		close $handles{$out2};
	}
}
