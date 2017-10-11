#!/usr/bin/env perl

use strict;
use Data::Dumper;

my %ERRORS = (
);

my %TEMPLATES = (
	'toT2' => '
	/** Convert the given value to {@code #T2}.
	 *
	 * @param a a number of {@code #T1} type.
	 * @return the equivalent value to {@code a} of {@code #T2} type.
	 */
	@Pure
	@Inline(value = "$3.pow(#I1, #I2)", imported = Math.class)
	public static #T2 to#UCT2(#T1 a) {
		return #CB;
	}
',
);

my %PRIMITIVES = (
	'double' => '#',
	'float' => '#',
	'long' => '#',
	'int' => '#',
	'short' => '#',
	'byte' => '#',
);

my %OBJECTS = (
	'Double' => 'Double.valueOf(#)',
	'Float' => 'Float.valueOf(#)',
	'Long' => 'Long.valueOf(#)',
	'Integer' => 'Integer.valueOf(#)',
	'Short' => 'Short.valueOf(#)',
	'Byte' => 'Byte.valueOf(#)',
);

my %ATOMICS = (
	'AtomicLong' => 'new AtomicLong(#)',
	'AtomicInteger' => 'new AtomicInteger(#)',
);

my %LEVEL = (
	'Double' => 1,
	'double' => 1,
	'Float' => 2,
	'float' => 2,
	'Long' => 3,
	'long' => 3,
	'AtomicLong' => 3,
	'Integer' => 4,
	'int' => 4,
	'AtomicInteger' => 4,
	'Short' => 5,
	'short' => 5,
	'Byte' => 6,
	'byte' => 6,
);

my @ACCESSORS = (
	'#.doubleValue()',
	'#.floatValue()',
	'#.longValue()',
	'#.intValue()',
	'#.shortValue()',
	'#.byteValue()',
);

my @RAW_CASTS = (
	'(double)#',
	'(float)#',
	'(long)#',
	'(int)#',
	'(short)#',
	'(byte)#',
);

my @RETURN_TYPES = (
	'double',
	'float',
	'long',
	'int',
	'short',
	'byte',
);

sub acc($$) {
	if ($PRIMITIVES{$_[0]}) {
		return $RAW_CASTS[$_[1] - 1];
	}
	return $ACCESSORS[$_[1] - 1];
}

sub apply2($$$$$$$) {
	my $r = "$_[0]";
	my $left = "$_[1]";
	my $right = "$_[2]";
	my $ucleft = ucfirst("$left");
	my $ucright = ucfirst("$right");
	my $racc1 = "$_[3]";
	my $racc2 = "$_[4]";
	my $crea1 = "$_[5]";
	my $crea2 = "$_[6]";

	my $acc1 = "$racc1";
	$acc1 =~ s/#/a/g;

	my $acc2 = "$racc2";
	$acc2 =~ s/#/b/g;

	$r =~ s/\Q#T1\E/$left/g;
	$r =~ s/\Q#T2\E/$right/g;
	$r =~ s/\Q#UCT1\E/$ucleft/g;
	$r =~ s/\Q#UCT2\E/$ucright/g;

	my $c1 = "$crea1";
	$c1 =~ s/#/$acc2/g;
	$r =~ s/\Q#CA\E/$c1/g;

	my $c2 = "$crea2";
	$c2 =~ s/#/$acc1/g;
	$r =~ s/\Q#CB\E/$c2/g;

	$r =~ s/\Q#A\E/$acc1/g;
	$r =~ s/\Q#B\E/$acc2/g;

	my $i1 = ($acc1) ? "(\$1)$acc1" : "\$1";
	$r =~ s/\Q#I1\E/$i1/g;
	my $i2 = ($acc2) ? "(\$2)$acc2" : "\$2";
	$r =~ s/\Q#I2\E/$i2/g;
	return $r;
}

while (my $file = glob("*CastExtensions.java")) {
	unlink($file) or die("$file: $!\n");
}

my @FILES = ();

my %ALLLEFTS = (%PRIMITIVES, %OBJECTS, %ATOMICS);

while (my ($left, $creaLeft) = each(%ALLLEFTS)) {
	local *FILE;
	my $basename = (($PRIMITIVES{$left}) ? "Primitive" . ucfirst($left) : ${left}). "CastExtensions";
	push @FILES, $basename;
	open(*FILE, "> ${basename}.java") or die("$!\n");
	print FILE "/*
 * \$Id\$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the \"License\");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an \"AS IS\" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// THIS FILE IS AUTO-GENERATED. DO NOT CHANGE MANUALLY

package io.sarl.lang.scoping.numbers;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static functions related to the casting of numbers of type {\@code $left}.
 *
 * \@author \$Author: sgalland\$
 * \@version \$FullVersion\$
 * \@mavengroupid \$GroupId\$
 * \@mavenartifactid \$ArtifactId\$
 * \@since 0.7
 * \@see \"https://github.com/eclipse/xtext-extras/issues/186\"
 */
\@SuppressWarnings(\"all\")
public final class ${basename} {

	private ${basename}() {
		//
	}

	// BEGIN GENERATED BLOCK\n";
	my %ALLRIGHTS = (%ATOMICS);
	if (!$PRIMITIVES{$left} && !$OBJECTS{$left}) {
		# Avoid to override outboxing.
		%ALLRIGHTS = (%ALLRIGHTS, %PRIMITIVES);
		while (my ($a, $b) = each(%OBJECTS)) {
			if (!$ALLRIGHTS{lc($a)}) {
				$ALLRIGHTS{$a} = $b;
			}
		}
	}
	while (my ($key, $res) = each(%TEMPLATES)) {
		while (my ($right, $creaRight) = each(%ALLRIGHTS)) {
			if (($left ne $right) &&
				(!$ERRORS{$key}{$left}{$right}) &&
				(!$ERRORS{$key}{'*'}{$right}) &&
				(!$ERRORS{$key}{$left}{'*'})) {
				my $levelLeft = $LEVEL{$left};
				my $levelRight = $LEVEL{$right};
				my $minLevel = ($levelLeft > $levelRight) ? $levelLeft : $levelRight;
				my $acc1 = acc($left, $minLevel);
				my $acc2 = acc($right, $minLevel);
				my $r = apply2("$res", $left, $right, $acc1, $acc2, $creaLeft, $creaRight);
				print FILE "$r";
			}
		}
	}

	print FILE "\n\t// END GENERATED BLOCK\n\n}\n";
	close(*FILE);
}

for my $file (@FILES) {
	print "\t\tfeatures.add($file.class);\n";
}

