#!/usr/bin/perl

use File::Find;
use File::Basename;
use Getopt::Long;

GetOptions( "stop"  => \$stop,
      "exhaustive"  => \$exhaustive,
      "help" => \$help);
if ($help){
  print "run-tests.pl [-stop][-exhaustive] test-suite...\n";
  exit(0);}
@param_list = @ARGV;
$module_path = '/home/popeeaco/personal/research/bugs-code';
#results can be: POSSIBLE|SAFETY|BUG
%runs = (
    "asian06" =>[
            ["examples/pepm08/bsearch","POSSIBLE","+infer PostStrong -m:2"],
            ["examples/pepm08/bubblesort","SAFETY","+infer PostStrong -m:2"],
            ["examples/pepm08/copyseq","SAFETY","+infer PostStrong -m:2"],
            ["examples/pepm08/dotprod","SAFETY","+infer PostStrong -m:2"],
            ["examples/pepm08/initarr","SAFETY","+infer PostStrong -m:2"],
            ["examples/pepm08/intro","POSSIBLE","+infer PostStrong -m:1"],
            ["examples/pepm08/intro2","POSSIBLE","+infer PostStrong -m:1"],
            ["examples/pepm08/mergesort","SAFETY","+infer PostStrong -m:4"],
            ["examples/pepm08/mvm","SAFETY","+infer +indir PostStrong -m:2"],
            ["examples/pepm08/queens","POSSIBLE","+infer PostStrong -m:2"],
            ["examples/pepm08/quicksort","POSSIBLE","+infer PostStrong -m:2"],
            ["examples/pepm08/sentinel","POSSIBLE","+infer PostStrong -m:1"],
            ["examples/pepm08/sumvec","SAFETY","+infer PostStrong -m:2"],
            ["examples/pepm08/swaparr","SAFETY","+infer PostStrong -m:1"],
            ["examples/pepm08/FFT","POSSIBLE","+infer PostStrong -m:3"],
            ["examples/pepm08/linpack","POSSIBLE","+infer PostStrong -m:2"],
            ["examples/pepm08/LU","POSSIBLE","+infer PostStrong -m:2"],
            ["examples/pepm08/SOR","SAFETY","+infer PostStrong -m:4"]
            ],
    "pepm08" =>[
            ["examples/pepm08/bsearch","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/bubblesort","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/copyseq","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/dotprod","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/initarr","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/intro","POSSIBLE","+infer StrongStrong -m:1"],
            ["examples/pepm08/intro2","POSSIBLE","+infer StrongStrong -m:1"],
            ["examples/pepm08/mergesort","SAFETY","+infer StrongWeak -m:1"],
            ["examples/pepm08/mvm","SAFETY","+infer +indir StrongStrong -m:1"],
            ["examples/pepm08/queens","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/quicksort","SAFETY","+infer StrongStrong -m:2"],
            ["examples/pepm08/sentinel","POSSIBLE","+infer StrongStrong -m:1"],
            ["examples/pepm08/sumvec","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/swaparr","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/FFT","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/linpack","POSSIBLE","+infer StrongWeak -m:2"],
            ["examples/pepm08/LU","SAFETY","+infer StrongStrong -m:1"],
            ["examples/pepm08/SOR","SAFETY","+infer StrongStrong -m:1"]
            ],
    "gulavani-fse06" =>[
            ["examples/gulavani-fse06/fse.fig1","SAFETYBUG","+infer DualStrong -m:1"],
            ["examples/gulavani-fse06/fse.fig3","SAFETY","+infer DualStrong -m:1"],
            ["examples/gulavani-fse06/fse.fig4","BUG","+infer DualStrong -m:1 +individual"],
            ["examples/gulavani-fse06/fse.fig6","SAFETY","+infer DualStrong -m:1"],
            ["examples/gulavani-fse06/fse.fig7","SAFETYBUG","+infer DualStrong -m:1 +individual"],
            ["examples/gulavani-fse06/fse.fig8","SAFETY","+infer DualStrong -m:1"],
            ["examples/gulavani-fse06/fse.fig9","BUG","+infer DualStrong -m:2 +individual"]],
    "fse08" => [
            ["examples/fse08/errbubblesort","BUG","+infer DualStrong -m:2 +simplifyCAbst +individual"],
            ["examples/fse08/errinitarr","BUG","+infer DualStrong -m:2 +individual"],
            ["examples/fse08/errquicksort","BUG","+infer DualStrong -m:2 +individual"],
            ["examples/fse08/errsentinel","BUG","+infer DualStrong -m:2 +individual"],
            ["examples/fse08/OK_SpamAssassin","BUG","+infer DualStrong -m:2 +individual"],
            ["examples/fse08/OK_samba","BUG","+infer DualStrong -m:1 +individual"],
            ["examples/fse08/CORRECTED_SpamAssassin","POSSIBLE","+infer DualStrong -m:2"],
            ["examples/fse08/CORRECTED_samba","SAFETY","+infer DualStrong -m:1"]]
);    

%result = ("SAFETY"=>0,"POSSIBLE"=>0,"BUG"=>0,"UNEXPECTED"=>0); 

$error_count = 0;
$error_files = "";
$output_file = "$module_path/log";
$iter_limit = 2;
$tests = 0;
$ptime = 0;

open(LOGFILE, "> $output_file") || die ("Could not open $output_file.\n");
if ($exhaustive) {
    @done = ();
    foreach $key (keys %runs) {
      $t_list = $runs{$key};
      foreach $test (@{$t_list}) {
          @done = push(@done,$test->[0]);
          print LOGFILE  "Checking $test->[0]\n";
          for($i=1;$i<=$iter_limit;$i++){
            check_one_file("$test->[0].imp",$i,"DualStrong" );
          }
          print LOGFILE "\n======================================\n";
      }
    } 
    if ($error_count > 0) {
      print LOGFILE "Total number of errors: $error_count in files: $error_files.\n";
    }
} else {
    foreach $param (@param_list) {
      $t_list = $runs{$param};  
      foreach $test (@{$t_list}) {
          $r = "";
          print "./imp $test->[0].imp $test->[2]\n";
          print LOGFILE "./imp $test->[0].imp $test->[2]\n";
          $output = `cd $module_path ;./imp $test->[0].imp $test->[2]`; 
          ($output =~ /Total CPU time: (\d*\.\d*) seconds/);
          $ptime = $ptime + $1;
          print LOGFILE "$output\n============\n";
          if ($output =~ /[\n]\s*POSSIBLE/){
            $r = "POSSIBLE";
            if($test->[1]=~ /POSSIBLE/) {
              $result{"POSSIBLE"} = $result{"POSSIBLE"} +1;
              $tests++;
            }
          }
          if($output =~ /[\n]\s*SAFETY/ ) {
            $r = $r."SAFETY";
            if($test->[1]=~ /SAFETY/) {
              $result{"SAFETY"} = $result{"SAFETY"} +1;
              $tests++;
            }
          }
          if($output =~ /[\n]\s*BUG/) {
            $r = $r."BUG";
            if($test->[1]=~ /BUG/) {
              $result{"BUG"} = $result{"BUG"} +1;
              $tests++;   
            }    
          }
          print "$r $test->[1]\n";
          print LOGFILE "$r $test->[1]\n";
          if($r!~ /^($test->[1])$/) {
            print LOGFILE "unexpected result for $param $test->[0]\n";
            $result{"UNEXPECTED"} = $result{"UNEXPECTED"} +1;
            $tests++;
          }
      }
    }
    print "------\n Total tests: $tests\n out of which:\n";
    print "\tPOSSIBLE: $result{'POSSIBLE'}\n\tSAFETY: $result{'SAFETY'}\n";
    print "\tBUG: $result{'BUG'}\n \tUNEXPECTED: $result{'UNEXPECTED'}\n";
    print LOGFILE "------\n Total tests: $tests\n out of which:\n";
    print LOGFILE "\tPOSSIBLE: $result{'POSSIBLE'}\n\tSAFETY: $result{'SAFETY'}\n";
    print LOGFILE "\tBUG: $result{'BUG'}\n \tUNEXPECTED: $result{'UNEXPECTED'}\n";
}
close(LOGFILE);
print "Total time: $ptime seconds\n";
exit(0);

sub check_one_file{
  $output = `cd $module_path ;./imp $_[0] +infer +check $_[2] -m:$_[1] 2>&1`;   
  ver_out($_[2],$_[1],$output);
  $output1 = `cd $module_path ;./imp $_[0] +infer +check -club:ConvexHull $_[2] -m:$_[1] 2>&1`;   
  print LOGFILE "\n ConvexHull";
  ver_out($_[2],$_[1],$output);
  $output2 = `cd $module_path ;./imp $_[0] +infer +check -simplifyCAbst $_[2] -m:$_[1] 2>&1`;   
  print LOGFILE "\n simplifyCAbst";
  ver_out($_[2],$_[1],$output);
  $output = `cd $module_path ;./imp con1.impt -infer +check 2>&1`;  
  if(($output !~ /[\n]\s*Pre\/Post checking...done/ ))#||($_[2] =~ "ERROR:" ))
        {print LOGFILE "!!!!!!!\t\t verifying the impt file (generated with : $_[2] -m:$_[1]), type checking not done\n";}
  $output = `cd $module_path ;gcc con1.c Primitives.c 2>&1`;  
  if (($output =~ /: error:/)|| ($output =~ /ld returned 1 exit status/))
    {print LOGFILE "!!!!!!!\t\t $_[2] -m:$_[1] gcc compile error\n";}
}

sub ver_out{
      $aux = 0;
      print LOGFILE "\n";
      if ($_[2] =~ /[\n]\s*POSSIBLE[^\n]*[\n]\s*(\d+)/ ){
        print LOGFILE "\t m:$_[1] $_[0] $1 runtime tests\n";
        $aux = 1;
        }
      elsif($_[2] =~ /[\n]\s*POSSIBLE([^=\n]*)=/ ){
        print LOGFILE "\t m:$_[1] $_[0] possible $1\n";
        $aux = 1;
        }
      elsif($_[2] =~ /[\n]\s*POSSIBLE[^\n]*[\n]\s*/ ){
        print LOGFILE "\t m:$_[1] $_[0] seems no precond?\n";
        $aux = 1;
        }
      if($_[2] =~ /[\n]\s*SAFETY/ ){
        print LOGFILE "\t m:$_[1] $_[0] safety\n";
        $aux = 1;
        }
      if($_[2] =~ /[\n]\s*BUG/ ){
        print LOGFILE "\t m:$_[1] $_[0] bug\n";
        $aux = 1;
        }
      if(($_[2] !~ /[\n]\s*Pre\/Post checking...done/ ))#||($_[2] =~ "ERROR:" ))
        {print LOGFILE "!!!!!!!\t\t type checking not done\n";}
      if ($_[2] =~ /ERROR:/)
        {print LOGFILE "!!!!!!!\t\t unexpected error\n";}
      if ($_[2] =~ /Parse error/)
        {print LOGFILE "!!!!!!!\t\t Parse error\n";}
      if($aux==0){
        if ($_[2]!~ /openFile: does not exist/)
          {print LOGFILE "!!!!!o eroare de formatare: $_[0] m:$_[1] bug\n"}
      }
}
