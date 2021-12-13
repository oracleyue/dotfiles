$pdf_mode = 1;
$bibtex_use = 2;

# tex engine
$pdflatex = "pdflatex -synctex=1 -shell-escape %O %S";
# $pdflatex = "xelatex -synctex=1 -shell-escape %O %S";  # for ctex
# $pdflatex = "lualatex -file-line-error %O %S";  # for gemini posters

# more: nomencl for thesis
# add_cus_dep( 'nlo', 'nls', 0, 'makenlo2nls' );
# sub makenlo2nls {
#     system( "makeindex -s nomencl.ist -o \"$_[0].nls\" \"$_[0].nlo\"" );
# }
# push @generated_exts, 'nlo', 'nls';

# add clean extensions
$clean_ext = "bbl synctex.gz";
