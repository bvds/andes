#!/usr/bin/perl -w
#
#  Special version of sort-by-time.pl to break up logs sent by alida
#  from the datashop.
#  Script to fix up bad lines after an hour:
#  perl -p -n -e 's/ ([1-9]:..:..\t)/ \n$1/g;' four_courses.txt > four_courses_fixed.txt
#
#  files containing Andes logs, as gotten from OLI, 
#  are not sorted by time.
#  This script sorts sessions in a log file by time

# Had to install this on FC6.
# On OS X, run existing cpan and install DateTime::Format::Strptime
use DateTime::Format::Strptime;
my $dateformat1 = DateTime::Format::Strptime->new( pattern => '%F %T' );
my $dateformat2 = DateTime::Format::Strptime->new( pattern => '%Y/%m/%d %T' );
my $dateformat3 = DateTime::Format::Strptime->new( pattern => '%B %d, %Y %T' );

my $date=DateTime->new(year=>1000);       # Any lines before the first session 

@brownh5=("bbrown","ben.askin","brownh5","george.liza","george.ned","gershmans","Halper.Mike","jayshah425","Jiang","Liu.J","MacTaggart","Mahal.tarika","mbaumont","mehta1","omeara.c","perry.david","Pflint25","priyanka.goyal","RWu","sivco","SZeng","Szymanski","Tanner.Geoffrey","testbrown","thorntonc","vaidya","weltnerb","wong.stephanie");

@brownh7=("109850","110300","111280","111840","111880","111981","akwok392","azhou93","bbrown","bchang93","cagardner223","cborinsky","cpdibari","DavidO","deliashen","emmanuel728\@yahoo.com","gdes","ivasudeva","jstupay","kkurls","Kwu","li0926li","Melinh","taylorbrown","tracy","Warshauer","zwass");

@gershmanH09=("111020","111850","112120","112161","112220","112400","112480","116660","117020","117410","117455","117815","118685","119090","124535","125400","127955","bng1312","ChrisHansen","ChrisM","g_g_gg_gg","k.jethwa","kjsepe","rnahar","sgershman","wesleyyiin");

@gershmanM09=("102500","102810","103920","104260","105085","106335","107265","108665","109130","109285","109670","113320","115250","119825","126800","Ajain","alexayuen","bkim92","bmerrill","brant_lai","bwu","cbattipaglia","cpang","crayy8","DaleZhang","djohanwow","flintiain1202","friedmanariele","gershmanM09","jbott12","jharvitt","kriney","lockwoodmeghan","marmanious","mattmosquera","michaelwu","millerm277","rrowe44","rugburn92","sgershman","tika","WBiesiadecki");

# Choose a section to map to.
%section= map { $_ => 1 } @brownh5;

while (<>) {
  if(/^(.*?)\t(.*)# Log of Andes session begun \w+, (.+) by /) {
    $inSection = $section{$1};
    if ($2) {
      my $tmp=$2; 
      chop $tmp; # remove trailing tab
      $date = ($dateformat1->parse_datetime($tmp)
	       or $dateformat2->parse_datetime($tmp));
    } else {
      $date=$dateformat3->parse_datetime($3);
      warn "Using local computer time $date for timestamp";
    }
  }
  if($inSection){
    $sessions{$date} .= $_;
  }
}

foreach $session (sort keys %sessions) {
  print "$sessions{$session}";
}
