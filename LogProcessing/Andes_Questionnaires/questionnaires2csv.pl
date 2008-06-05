#!/usr/bin/perl -w
# Copyright (c) 2006 Robert G.M. Hausmann
#
# The purpose of this script is to convert online Andes Questionnaires 
# into csv format.
# Usage: 
#    questionnaires_to_excel.pl andes_q.txt > andes_q.csv
#    In the spreadsheet, do a regexp find and replace to remove single
#    quote at beginning of line (used to force text format),
#    then sort first 3 columns.  Make School Section CourseID Name columns
#    explcitly text format.  (I could only get this to work in 
#    openoffice and then export to excel.)
# Mathematica format:
#    questionnaires_to_excel.plx -m andes_q.txt > andes_q.m
# Open-ended questions instead use -o

use Getopt::Long;
my $mma=0;
my $openended=0;
GetOptions ('math|m|mma' => \$mma,'o' => \$openended);

my @categories = $openended?(
'Year','Semester','School','Section','CourseID','Name','Date','Time',
'1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25'
):(
'Year','Semester','School','Section','CourseID','Name','Date','Time',
'1','2','3','4','5','6','7','8A','8B','9','10','11A','11B','11C','11D','11E','11F','11G','11H','11I','11J','11K','11L','12A','12B','12C','12D','12E','13A','13B','13C','13D','14','15','16A','16B','16C','16D','16E','17','18A','18B','18C','18D','19','20','21','22','23A','23B','23C','23D','23E','24','25','26','27','28','29','30','30A','30B','30C','30D'
);


# for sanity test later on
my %categories_hash;
foreach (@categories) { $categories_hash{$_}=1; }

my %encoding = (
'bobh' => 'bobh',
'tmwilbur' => 'tmwilbur',
'xxxxx' => 'xxxxx',
'yyyy' => 'yyyy',
'TTTTT' => 'TTTTT',
'bobh' => 'bobh',
'bobh' => 'bobh',
'092178' => 'DDA2D',
'092586' => 'DD895',
'm096378' => 'DC9C5',
'm093546' => 'DD4D5',
'095370' => 'DCDB5',
'095730' => 'DCC4D',
'090600' => 'DE057',
'094836' => 'DCFCB',
'095532' => 'DCD13',
'081308' => 'E04A3',
'090366' => 'DE141',
'091650' => 'DDC3D',
'094626' => 'DD09D',
'092808' => 'DD7B7',
'094968' => 'DCF47',
'096342' => 'DC9E9',
'093756' => 'DD403',
'095400' => 'DCD97',
'm090228' => 'DE1CB',
'090816' => 'DDF7F',
'093216' => 'DD61F',
'095874' => 'DCBBD',
'092244' => 'DD9EB',
'rommelk' => 'rommelk',
'096708' => 'DC87B',
'093822' => 'DD3C1',
'091608' => 'DDC67',
'091620' => 'DDC5B',
'090756' => 'DDFBB',
'091704' => 'DDC07',
'095364' => 'DCDBB',
'095418' => 'DCD85',
'093564' => 'DD4C3',
'097140' => 'DC6CB',
'095826' => 'DCBED',
'096060' => 'DCB03',
'097134' => 'DC6D1',
'm092142' => 'DDA51',
'092478' => 'DD901',
'094200' => 'DD247',
'093990' => 'DD319',
'096600' => 'DC8E7',
'092904' => 'DD757',
'094104' => 'DD2A7',
'094062' => 'DD2D1',
'096618' => 'DC8D5',
'092406' => 'DD949',
'mayorj' => 'mayorj',
'ludviceke' => 'ludviceke',
'freys' => 'freys',
'hutchisonl' => 'hutchisonl',
'davom87' => 'davom87',
'hultins' => 'hultins',
'TubmanW' => 'TubmanW',
'kinchen' => 'kinchen',
'Steven Chong' => 'Steven Chong',
'phy111' => 'phy111',
'kal srour' => 'kal srour',
'David Enosr' => 'David Enosr',
'kassemis' => 'kassemis',
'popek' => 'popek',
'096168' => 'DCA97',
'094326' => 'DD1C9',
'095556' => 'DCCFB',
'095688' => 'DCC77',
'095496' => 'DCD37',
'091188' => 'DDE0B',
'm094416' => 'DD16F',
'093462' => 'DD529',
'm091764' => 'DDBCB',
'093012' => 'DD6EB',
'094068' => 'DD2CB',
'095262' => 'DCE21',
'092784' => 'DD7CF',
'091494' => 'DDCD9',
'095892' => 'DCBAB',
'090894' => 'DDF31',
'094116' => 'DD29B',
'092730' => 'DD805',
'091728' => 'DDBEF',
'096786' => 'DC82D',
'091014' => 'DDEB9',
'091788' => 'DDBB3',
'094236' => 'DD223',
'096054' => 'DCB09',
'090888' => 'DDF37',
'm090234' => 'DE1C5',
'092988' => 'DD703',
'095082' => 'DCED5',
'097290' => 'DC635',
'096042' => 'DCB15',
'096396' => 'DC9B3',
'094428' => 'DD163',
'090252' => 'DE1B3',
'095322' => 'DCDE5',
'093432' => 'DD547',
'm093036' => 'DD6D3',
'096096' => 'DCADF',
'090594' => 'DE05D',
'093519' => 'DD4F0',
'093090' => 'DD69D',
'095736' => 'DCC47',
'094470' => 'DD139',
'090672' => 'DE00F',
'saedw' => 'saedw',
'tanisha bogans' => 'tanisha bogans',
'dymestatus' => 'dymestatus',
'djwaclawski' => 'djwaclawski',
'masonj' => 'masonj',
'hmmiller1' => 'hmmiller1',
'selzerb' => 'selzerb',
'devonstanko' => 'devonstanko',
'cspence' => 'cspence',
'Holetsr' => 'Holetsr',
'abrahamd' => 'abrahamd',
'selzerb' => 'selzerb',
'beenen' => 'beenen',
'koepkem' => 'koepkem',
'mlewis' => 'mlewis',
'mgilbert' => 'mgilbert',
'catoirea' => 'catoirea',
'longa' => 'longa',
'095178' => 'DCE75',
'm094680' => 'DD067',
'096024' => 'DCB27',
'091968' => 'DDAFF',
'097056' => 'DC71F',
'094098' => 'DD2AD',
'cultrab' => 'cultrab',
'091782' => 'DDBB9',
'athertonj' => 'athertonj',
'Dursteina' => 'Dursteina',
'094050' => 'DD2DD',
'096702' => 'DC881',
'chlebekr' => 'chlebekr',
'092508' => 'DD8E3',
'bkroeger' => 'bkroeger',
'adambergoo' => 'adambergoo',
'Jason.Koller' => 'Jason.Koller',
'Justin.Borgerding' => 'Justin.Borgerding',
'schuettec' => 'schuettec',
'dunnb' => 'dunnb',
'clintonwalls' => 'clintonwalls',
'GnazzoD' => 'GnazzoD',
'mcguirev' => 'mcguirev',
'EverettJ' => 'EverettJ',
'montgomery e3' => 'montgomery e3',
'091194' => 'DDE05',
'hockingn' => 'hockingn',
'091326' => 'DDD81',
'097260' => 'DC653',
'093870' => 'DD391',
'm093072' => 'DD6AF',
'095898' => 'DCBA5',
'm093072' => 'DD6AF',
'090942' => 'DDF01',
'092436' => 'DD92B',
'091062' => 'DDE89',
'090846' => 'DDF61',
'wardm' => 'wardm',
'hinesj' => 'hinesj',
'favingerm' => 'favingerm',
'tortugad' => 'tortugad',
'morinc' => 'morinc',
'fedorchikm' => 'fedorchikm',
'weilerj' => 'weilerj',
'aaron.price' => 'aaron.price',
'walkerv' => 'walkerv',
'burdittr' => 'burdittr',
'scheglovm' => 'scheglovm',
'hans' => 'hans',
'c10tyler.beal' => 'c10tyler.beal',
'kstaples' => 'kstaples',
'pakm' => 'pakm',
'HarmonM' => 'HarmonM',
'trann' => 'trann',
'skellyd' => 'skellyd',
'rubinc' => 'rubinc',
'blankenbergera' => 'blankenbergera',
'bassettn' => 'bassettn',
'willetr' => 'willetr',
'Ben Truong' => 'Ben Truong',
'Allison' => 'Allison',
'srtaylor2' => 'srtaylor2',
'jrmasimore' => 'jrmasimore',
'tjcolvin' => 'tjcolvin',
'mcdowells' => 'mcdowells',
'weberk' => 'weberk',
'yeea' => 'yeea',
'Jason Bennett' => 'Jason Bennett',
'martinw' => 'martinw',
'Awkinsley' => 'Awkinsley',
'danmarley' => 'danmarley',
'ludwigm' => 'ludwigm',
'illigj' => 'illigj',
'quacoa' => 'quacoa',
'daporter' => 'daporter',
'sunigas' => 'sunigas',
'nettism' => 'nettism',
'hartt' => 'hartt',
'Derek' => 'Derek',
'Sarah' => 'Sarah',
'dmattei' => 'dmattei',
'fullertonm' => 'fullertonm',
'hmmiller1' => 'hmmiller1',
'hmmiller1' => 'hmmiller1',
'C10Gregory.Edmonds' => 'C10Gregory.Edmonds',
'weakleym' => 'weakleym',
'masiellom' => 'masiellom',
'poolee' => 'poolee',
'azabs' => 'azabs',
'wojciechowiczm' => 'wojciechowiczm',
'sprangerk' => 'sprangerk',
'jordym' => 'jordym',
'landeckerz' => 'landeckerz',
'pakm' => 'pakm',
'favingerm' => 'favingerm',
'c10ralph.hale' => 'c10ralph.hale',
'watsonb' => 'watsonb',
'depaolisd' => 'depaolisd',
'peterss' => 'peterss',
'lopezj' => 'lopezj',
'ottenadr' => 'ottenadr',
'jschafer' => 'jschafer',
'roloffz' => 'roloffz',
'WrightN' => 'WrightN',
'avrettj' => 'avrettj',
'walzj' => 'walzj',
'hopej' => 'hopej',
'HartmanC' => 'HartmanC',
'dominick' => 'dominick',
'Totino' => 'Totino',
'sullivanb' => 'sullivanb',
'williamsr' => 'williamsr',
'richardsond' => 'richardsond',
'mccarthyk' => 'mccarthyk',
'vantimmerena' => 'vantimmerena',
'mohrh' => 'mohrh',
'burdittr' => 'burdittr',
'c10christopher.edlund' => 'c10christopher.edlund',
'bobh' => 'bobh',
'123' => '123',
'bobh' => 'bobh',
'12345' => '12345',
'113654' => 'D8649',
'106042' => 'DA405',
'106576' => 'DA1EF',
'102856' => 'DB077',
'112064' => 'D8C7F',
'101350' => 'DB659',
'104470' => 'DAA29',
'102130' => 'DB34D',
'102379' => 'DB254',
'106636' => 'DA1B3',
'102334' => 'DB281',
'106882' => 'DA0BD',
'102310' => 'DB299',
'102640' => 'DB14F',
'104170' => 'DAB55',
'101572' => 'DB57B',
'm104434' => 'DAA4D',
'102130' => 'DB34D',
'm102052' => 'DB39B',
'105382' => 'DA699',
'105988' => 'DA43B',
'100162' => 'DBAFD',
'100702' => 'DB8E1',
'101782' => 'DB4A9',
'101188' => 'DB6FB',
'105700' => 'DA55B',
'105982' => 'DA441',
'105922' => 'DA47D',
'm101902' => 'DB431',
'101800' => 'DB497',
'107002' => 'DA045',
'102214' => 'DB2F9',
'105274' => 'DA705',
'100198' => 'DBAD9',
'105244' => 'DA723',
'102580' => 'DB18B',
'105082' => 'DA7C5',
'106432' => 'DA27F',
'101260' => 'DB6B3',
'106510' => 'DA231',
'105835' => 'DA4D4',
'106402' => 'DA29D',
'100228' => 'DBABB',
'103174' => 'DAF39',
'106618' => 'DA1C5',
'102766' => 'DB0D1',
'100660' => 'DB90B',
'106336' => 'DA2DF',
'105853' => 'DA4C2',
'104494' => 'DAA11',
'106270' => 'DA321',
'101290' => 'DB695',
'102091' => 'DB374',
'104476' => 'DAA23',
'100378' => 'DBA25',
'101602' => 'DB55D',
'102064' => 'DB38F',
'100252' => 'DBAA3',
'102568' => 'DB197',
'102538' => 'DB1B5',
'100468' => 'DB9CB',
'102832' => 'DB08F',
'105172' => 'DA76B',
'101818' => 'DB485',
'105232' => 'DA72F',
'101122' => 'DB73D');

 
# print out header line
{
  local $"=",";  # comma formatted lists
  my @quoted_categories =  map {"\"" . $_ . "\""} (@categories);
  print "rawquestions={{" if $mma;
  print "@quoted_categories";
  print "}," if $mma;
  print "\n";
}

while (<>) {  #loop through mails
  my %answers=();
  my $last=0;

  while (<>) {  # loop through questionnaire lines
    last if /^.From andes2\@pitt.edu/;
    
    0 and warn "working on $_";
    my $keep = $openended?'Open-Ended_':'Question_';
    my $discard = $openended?'Question_':'Open-Ended_';

    # Find the answers to the close-ended questions.
    if (/^$keep(\w+)=(.*)/) {
      $answers{$1} = $2;
      $last=$1; 
    }
    # throw away Open-Ended fields
    elsif (/^$discard(\w+)=(.*)/) { 
	$last=0;
    }
    # get non-question fields (Name, School, etc.)
    elsif (/^(\w+)=(.*)/) {
      $answers{$1} = $2;
      $last=0;
    }
    #  Sometimes essay answers take up multiple lines, paste to most recent. 
    elsif ($last) {
       $answers{$last} .= $_;
    }
    # Date: Tue, 27 Nov 2007 13:39:02 -0500 (EST)
    elsif (/^Date: \w+, (\d+ \w+) (\d+) (\d+:\d+:\d+) /) {
      $answers{'Date'} = $1 . ' ' .$2;
      $answers{'Year'} = $2;
      $answers{'Time'} = $3;
    } else {
      $last=0;
    }
  }

  # Test that all keys are matched
  foreach my $key (keys %answers) {
    warn "unmatched category $key" unless $categories_hash{$key};
  }

  #  Hash table of encodings:  this is supposed to be supplied
  #  by anders.
  $answers{'Name'} = $encoding{$answers{'Name'}} if 
    $answers{'Name'} and $encoding{$answers{'Name'}};

  my $count=0;
  print "{" if $mma;
  foreach (@categories) {
    if ($count++) { print ",";}
    my $x=$answers{$_};
    next unless $x;
    chomp $x;  # remove CR; in unix, we don't have \r to worry about
    # match a number unless it is a student name
    if ($x =~ /^[+-]?[\d.]+$/ and not /Name/) {
      print "$x";
    }
    else {  #quote everything else
      if ($mma) {
	$x =~ s/\n/\\n/g; # escape newline
	$x =~ s/"/\\"/g;  # escape quote
      } else {
	$x =~ s/"/""/g; # for csv, double any quotes
	# force text format in spreadsheet
	$x = "'" . $x unless /Date/ or /Time/;
      }
      print "\"$x\"";
    }
  }
  print "}" if $mma;
  last if eof;  # when inner loop reaches eof
  print "," if $mma;
  print "\n";

}

print "};" if $mma;
print "\n";

