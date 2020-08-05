#! /usr/bin/perl

use v5.18;
use common::sense;
use utf8;
binmode( DATA, ":encoding(UTF-8)" );
use open qw( :encoding(UTF-8) :std );
use Getopt::Long;
use File::Slurper qw(read_text write_text read_dir);
use Data::Dumper qw(Dumper);
use Sort::Key qw(keysort);
use Pod::Usage;
use List::Util qw(any);

my ( $opt_d, $opt_f, @exclude_tags, @exclude_cats, @ignore_tags, %opt );

$opt{suffix} = '.txt';

GetOptions(
	'd|directory=s'                     => \$opt_d,
	'f|tags-file=s'                     => \$opt_f,
	'et|exclude-tag=s'                  => \@exclude_tags,
	'ec|exclude-cat|exclude-category=s' => \@exclude_cats,
	'it|ignore-tag=s'                   => \@ignore_tags,
	'tag-index=s'                       => \$opt{tag_index},
	'cat-index|category-index=s'        => \$opt{cat_index},
	'tags-prefix=s'                     => \$opt{tags_prefix},
	'cats-prefix=s'                     => \$opt{cats_prefix},
	'no-header'                         => \$opt{no_header},
	'no-header-single'                  => \$opt{no_header_single},
	'list-format'                       => \$opt{list_format},
	'v|verbose'                         => \$opt{verbose},
	'suffix=s'                          => \$opt{suffix},
	'h|help'                            => \$opt{help},
) or pod2usage( -exitval => 1 );

pod2usage( -exitval => 0 ) if $opt{help};

my $file_regexp = '\.(txt)$';

# part 1: read files
my @files;

foreach my $filename ( read_dir $opt_d ) {
	next unless $filename =~ /$file_regexp/;

	my %headers = &parse_headers( $opt_d . '/' . $filename );

	my $docname = &clean_doc_name($filename);
	$docname =~ s/\.txt$//;

	my $file = {
		filename   => $filename,
		docname    => $docname,
		title      => $headers{title} // $docname,
		tags       => $headers{tags},
		categories => $headers{categories},
	};

	if (@ignore_tags) {
		next if map {
			my $a = $_;
			grep { $a eq $_ } @ignore_tags
		} @{ $file->{tags} };
	}

	push @files, $file;
}

# part 2: list by tag and cat
my ( %by_tag, %by_cat );

foreach my $file ( keysort { $_->{title} } @files ) {
	foreach my $tag ( @{ $file->{tags} } ) {
		next if grep { $_ eq $tag } @exclude_tags;
		push( @{ $by_tag{$tag} }, $file );
	}

	foreach my $cat ( @{ $file->{categories} } ) {
		next if grep { $_ eq $cat } @exclude_cats;
		push( @{ $by_cat{$cat} }, $file );
	}
}

# part 3: print as needed

if ( $opt{tag_index} ) {
	my $filename = clean_doc_name("$opt_d/$opt{tag_index}$opt{suffix}");
	&print_file(
		filename => $filename,
		template => "title: omg\ntags: system\n\nbody\n",
		title    => "Tags",
		data     => &pretty_all(
			items  => \%by_tag,
			key    => 'tags',
			title  => 'Tags',
			prefix => 'tag_'
		),
	) && ( !$opt{verbose} or say "$filename written" );
}

if ( $opt{cat_index} ) {
	my $filename = clean_doc_name("$opt_d/$opt{cat_index}$opt{suffix}");
	&print_file(
		filename => $filename,
		template => "title: omg\ntags: system\n\nbody\n",
		title    => "Categorías",
		data     => &pretty_all(
			items  => \%by_cat,
			key    => 'categories',
			title  => 'Categorías',
			prefix => 'category_'
		),
	) && ( !$opt{verbose} or say "$filename written" );
}

if ( $opt{tags_prefix} ) {
	foreach my $key ( sort keys %by_tag ) {
		my $filename = clean_doc_name("$opt_d/$opt{tags_prefix}$key$opt{suffix}");
		&print_file(
			filename => $filename,
			template => "title: omg\ntags: system\n\nbody\n",
			title    => "Tag: $key",
			data     => &pretty_single(
				items        => $by_tag{$key},
				title_prefix => "Tag: ",
				title        => $key,
			),
		) && ( !$opt{verbose} or say "$filename written" );
	}
}

if ( $opt{cats_prefix} ) {
	foreach my $key ( keys %by_cat ) {
		my $fnkey = "$key";
		$fnkey =~ s/ /_/g;
		$fnkey =~ s/ñ/n/g;
		my $filename = clean_doc_name("$opt_d/$opt{cats_prefix}$fnkey$opt{suffix}");
		print STDERR "$filename\n" if $opt{verbose};
		&print_file(
			filename => $filename,
			template => "title: omg\ntags: system\n\nbody\n",
			title    => "Categoría: $key",
			data     => &pretty_single(
				items        => $by_cat{$key},
				title_prefix => "Categoría: ",
				title        => $key,
			),
		) && ( !$opt{verbose} or say "$filename written" );
	}
}

exit;

sub pretty_single {

	my %arg = @_;

	my @lines;
	push( @lines, ( "* $arg{title_prefix}$arg{title}", '' ) ) unless $opt{no_header_single};

	foreach my $file ( @{ $arg{items} } ) {
		push @lines, "- [[$file->{docname}][$file->{title}]]";
	}

	if ( $opt{list_format} ) {
		@lines = map { "  $_" } @lines;
		unshift @lines, "- $arg{title}";
	}

	join "\n", @lines, '', '';
}

sub pretty_all {

	my (%arg) = @_;

	my @lines;
	push( @lines, ( "* $arg{title}", '' ) ) unless $opt{no_header};

	foreach my $item ( sort keys %{ $arg{items} } ) {
		my $count = scalar @{ $arg{items}->{$item} };
		my $doc   = &clean_doc_name( $arg{prefix} . $item );
		push @lines, "- [[$doc][$item]] ($count)";
	}

	if ( $opt{list_format} ) {
		@lines = map { "  $_" } @lines;
		unshift @lines, "- $arg{title}";
	}

	join "\n", @lines, '', '';
}

sub tokenize {

	my @tokens;

	foreach (@_) {
		my @m = m/"([^"]+)"|(?<=^)([^"]\S+)|(?<=\s)([^"]\S+)/g;
		push @tokens, grep { defined } @m;
	}

	return \@tokens;

}

sub parse_headers {

	my ($filename) = @_;

	my $file = read_text($filename);

	my ($headers) = split /\n\n/, $file;

	my %headers;

	foreach my $pair ( split /\n/, $headers ) {

		chomp $pair;

		my ( $key, $value ) = split /\s*:\s*/, $pair, 2;

		$key = lc($key);

		$key   =~ s/^\s*|\s*$//g;
		$value =~ s/^\s*|\s*$//g;

		$value = &tokenize($value) if ( $key eq 'tags' || $key eq 'categories' );

		$headers{$key} = $value;
	}

	return %headers;
}

sub clean_doc_name {

	my ($name) = @_;

	$name = lc($name);
	$name =~ s/ /_/g;

	$name =~ tr/áéíóúüñ/aeiouun/;

	return $name;

}

sub print_file {

	my %arg = @_;

	my $file = "$arg{template}";

	$file =~ s/^title:.*?\n/title: $arg{title}\n/ms;

	$file =~ s/\n\n.*$/\n\n$arg{data}/s;

	write_text( $arg{filename}, $file );

}

__END__

=head1 NAME

get_tags.pl - build tags file

=head1 SYNOPSIS

get_tags.pl -d <directory> [options]

=head1 OPTIONS

=over 8

=item B<--directory|d>

Documents directory to parse.

=item B<--tag-index> [file]

File to write tag catalog output to.

=item B<--cat-index> [file]

File to write category catalog output to.

=item B<--tags-prefix> [prefix]

String to prepend to tag listing file, try B<tag_>

=item B<--cats-prefix> [prefix]

String to prepend to category listing file, try B<category_>

=item B<--suffix> [suffix]

Suffix or filename extension. Defaults to B<.txt>

=item B<--ignore-tag> [tag]

Don't process at all files tagged with given tag.

=back

=cut

