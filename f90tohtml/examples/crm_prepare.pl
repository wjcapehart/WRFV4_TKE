#!/usr/bin/perl
#6 April 2009
#change the following ABSOLUTE PATH to your CRM directory
$the_path="/home/bfiedler/f90tohtml/examples/crm-2.1.2-ccm-3.6/";

-e $the_path || die ("$the_path not set right");

$outdir="crm_ls"; #relative path to output directory
mkdir($outdir,0755) if (! -e $outdir);


@the_dirs=(	"src/ccmlsm_share/",
		"src/control/",
		"src/crm/",
		"src/csm_share/",
		"src/dom/",
		"src/eul/",
		"src/physics/",
		"src/srchutil/"
			);

$the_files="*.[fF]";
foreach $dir (@the_dirs){
	@split_dir=split /\//,$dir;
	$title=pop @split_dir;
	if ($title eq "") { $title="main"};
	$file_name="$outdir/".$title.".ls";
	$ls_opt=$the_path.$dir.$the_files;
	print "\npreparing $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >$file_name " || die ("cannot write $file_name \n");
}
$file_name="$outdir/include.ls";
unlink($file_name) || warn ("cannot unlink $file_name");
$the_files="*.h";
foreach $dir (@the_dirs){
	$ls_opt=$the_path.$dir.$the_files;
	print "\nappending to $file_name\n\t from $ls_opt\n";
	system "ls $ls_opt >>$file_name " || die ("darn, no write $file_name \n");
}
print "DONE\nNow try typing f90tohtml crm.f2h\n";

