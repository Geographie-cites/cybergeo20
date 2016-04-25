use warnings;
use strict;

nettoyage_cybergeo();

sub nettoyage_cybergeo {
    my @fichiers;
    push @fichiers, glob("../Data/raw/texts/*text.txt");
    push @fichiers, "../Data/raw/cybergeo.csv";
    nettoyage_fichier($_) for @fichiers;
}

sub nettoyage_fichier {
	my $fichier = shift;
	my $fh;
	open $fh, '<', $fichier or die "Impossible d'ouvrir en lecture $fichier";
	warn sprintf "Traitement du fichier %s\n", $fichier;
	my $texte;
    while (<$fh>) {
    	chomp;
    	my $line = nettoyage($_);
		$texte .= sprintf "%s\n", $line;
    }
	close $fh;
	open $fh, '>', $fichier or die "Impossible d'ouvrir en écriture $fichier";
	print $fh $texte;
	close $fh;
	return;
}

sub nettoyage {
	my $chaine = shift;
   	$chaine =~ s/´/'/g;            # Apostrophes (normalisation)
    $chaine =~ s/°/ /g;             # Caractères spéciaux
  	$chaine =~ s/ / /g;            # Espace insécable
   	$chaine =~ s/<br\s+?\/?>/ /g;  # Retour à la ligne
   	$chaine =~ s/<\/?\w+>/ /g;     # Balise XML simple
   	$chaine =~ s/<a\s+[^>]+>/ /g;  # Balise XML pour une référence
   	$chaine =~ s/&nbsp;/ /g;       # Espace insécable
   	$chaine =~ s/&\w+;/ /g;        # Autres caractères XML
   	$chaine =~ s/&/et/g;           # Esperluette
   	$chaine =~ s/\s+/ /g;          # Réduction du nombre d'espaces multiples
	return $chaine;
}