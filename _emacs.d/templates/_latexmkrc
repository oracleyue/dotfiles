$pdf_mode = 1;

# tex engines
$pdflatex = "pdflatex -synctex=1 -shell-escape %O %S";
#$pdflatex = "xelatex -synctex=1 -shell-escape %O %S";

# custom dependency and function for nomencl package
add_cus_dep( 'nlo', 'nls', 0, 'makenlo2nls' );
sub makenlo2nls {
    system( "makeindex -s nomencl.ist -o \"$_[0].nls\" \"$_[0].nlo\"" );
}
push @generated_exts, 'nlo', 'nls';

# add clean extensions
$clean_ext .= ' %R.bbl';
