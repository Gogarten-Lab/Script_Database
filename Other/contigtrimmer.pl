#!/usr/bin/perl -w

use strict;
 

my $infasta=$ARGV[0];
 
open FH1 , "$infasta" or die "$!";



my @sequence ="";
my $first = "";
my $second ="";
my $begin="";
my $end="";
my $front = "";
my $back = "";
my $final = "";
while(<FH1>){
	chomp;
	@sequence = split /\t/, $_;
	$first = $sequence[6];
	$second = $sequence[7];
	if ($first > $second) {
		 $begin = $second;
 		 $end = $first;
	}
	else{
		$begin = $first;
		$end = $second;
	}
	my $frontflank = int($begin)-1000;
	my $backflank = int($end+1000);
	my $seqname = $sequence[0];
	open FH2, "$seqname".".seqfile";
	my $worthless ="";
	my $cds= "";
	while(<FH2>){
		chomp;
		if(/^\>(.*)/){
			$worthless = $worthless . $_;
		}
		else{
			$cds = $cds . $_;
		}
	}
	
	my @readin = split //, $cds;
	
	
	while ($frontflank < $backflank){
		$front = $front . $readin[$frontflank];
		$frontflank += 1;
	
	}
	
	$final = $front ;
	
	print ">".$sequence[0].".seqfile2\n".$final."\n";
	@sequence ="";
	$front ="";
	$final ="";
	$back ="";
	$first = "";
	$second ="";
	$begin="";
	$end="";
}
