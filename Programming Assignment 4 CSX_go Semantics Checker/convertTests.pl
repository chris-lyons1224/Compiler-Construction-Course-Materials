#!/usr/bin/perl -w

# convert.pl: takes CSX input and converts to CSX_go.
# Raphael Finkel 6/2018

use strict;

sub pass1 {
	my @result;
	while (my $line = <STDIN>) {
		chomp $line;
		if ($line =~ /Created/) {
			$line .= '; Modified Jun 2018';
		} elsif ($line =~ /^\s*class\s+(\w+)\s*{/) {
			$line = "package $1";
		} elsif ($line =~ /^(\s*)(int|char|bool)\s+(\w+)(\[\d+\])?\s*(=\s*.*)?;/) {
			my ($space, $type, $name, $bounds, $init) = ($1, $2, $3, $4, $5);
			$init = '' unless defined $init;
			$bounds = '' unless defined $bounds;
			$line = "${space}var $name $type$bounds $init;";
		} elsif ($line =~ /^}/) {
			next; # omit last brace
		} elsif ($line =~ /(\s+)(int|char|bool|void)\s+(\w+)\((.*)\)\s*{/) {
			my ($space, $type, $name, $formals) = ($1, $2, $3, $4);
			$type = '' if $type eq 'void';
			my @convertedFormals;
			for my $formal (split /,\s*/, $formals) {
				$formal =~ /(int|char|bool)\s+(.+)/ or do {
					warn("can't handle formal $formal\n");
					next;
				};
				my ($formalType, $formalName) = ($1, $2);
				push @convertedFormals, "$formalName $formalType";
			}
			$line = "func $name(" . join(', ', @convertedFormals) .
				") $type {";
		} elsif ($line =~ /(\s+)(if|while)\s*\((.*)\)(.*)/) {
			my ($spaces, $stmt, $condition, $remainder) = ($1, $2, $3, $4);
			$stmt = 'for' if $stmt eq 'while';
			unless ($remainder =~ /^\s*\{/) {
				$remainder = "{\n" . <STDIN> . "${spaces}\}";
			}
			$line = "${spaces}$stmt $condition $remainder";
		} elsif ($line =~ /^(\s*)(.*)(\s)else\s*(.*)$/) {
			my ($spaces, $first, $delimiter, $remainder) = ($1, $2, $3, $4);
			if ($remainder eq '') {
				$line .= " {\n" . <STDIN> . "$spaces$delimiter}";
			} elsif ($remainder !~ /\{/) {
				$line = "$first else { $remainder }";
			}
		}
		$line =~ s/\((bool|char|int)\)\s+\(([^\)]+)\)/$1($2)/g; # casts
		$line =~ s/\((bool|char|int)\)\s+([^ ,]+)/$1($2)/g; # casts
		$line =~ s/\s*;/;/;
		$line =~ s/ +$//; # trim
		push @result, $line;
	} # each line
	return @result;
} # pass1

sub pass2 { # multi-line fixes
	my (@lines) = @_;
	my $text = join("\n", @lines);
	$text =~ s/}\s*\n\s*else/} else/g;
	return $text
} # pass2

sub pass3 { # go formatting
	my ($text) = @_;
	$text =~ s/##(.*)##/\/*$1*\//sg; # for gofmt
	$text =~ s/~/-/g; # for gofmt
	open GOFMT, "|gofmt";
	print GOFMT $text;
	close GOFMT;
} # pass3

my @lines;
@lines = pass1(); # most conversions
print pass2(@lines); # multi-line fixes, go formatting
