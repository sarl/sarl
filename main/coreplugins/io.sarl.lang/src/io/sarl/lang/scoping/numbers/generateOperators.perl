#!/usr/bin/env perl

use strict;
use Data::Dumper;

my %MONO_ERRORS = (
	'operator_minus' => {
		'Double' => 1,
		'double' => 1,
		'float' => 1,
		'long' => 1,
		'int' => 1,
		'short' => 1,
		'byte' => 1,
	},
);
my %BI_ERRORS = (
);

my %MONO_TEMPLATES = (
	'operator_minus' => '
	/** The unary {@code minus} operator. This is the equivalent to
	 * the Java\'s {@code -} function.
	 *
	 * @param i a number.
	 * @return {@code -i}
	 */
	@Pure
	@Inline(value = "(-#I)", constantExpression=true)
	public static #RT operator_minus(#T i) {
		return -#A;
	}
',
);

my %BI_TEMPLATES = (
	'operator_plus' => '
	/** The binary {@code plus} operator. This is the equivalent to
	 * the Java {@code +} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a+b}
	 */
	@Pure
	@Inline(value = "(#I1 + #I2)", constantExpression=true)
	public static #RT operator_plus(#T1 a, #T2 b) {
		return #A + #B;
	}
',
	'operator_minus' => '
	/** The binary {@code minus} operator. This is the equivalent to
	 * the Java {@code -} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a-b}
	 */
	@Pure
	@Inline(value = "(#I1 - #I2)", constantExpression=true)
	public static #RT operator_minus(#T1 a, #T2 b) {
		return #A - #B;
	}
',
	'operator_multiply' => '
	/** The binary {@code multiply} operator. This is the equivalent to
	 * the Java {@code *} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a*b}
	 */
	@Pure
	@Inline(value = "(#I1 * #I2)", constantExpression=true)
	public static #RT operator_multiply(#T1 a, #T2 b) {
		return #A * #B;
	}
',
	'operator_divide' => '
	/** The binary {@code divide} operator. This is the equivalent to
	 * the Java {@code /} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a/b}
	 */
	@Pure
	@Inline(value = "(#I1 / #I2)", constantExpression=true)
	public static #RT operator_divide(#T1 a, #T2 b) {
		return #A / #B;
	}
',
	'operator_modulo' => '
	/** The binary {@code modulo} operator. This is the equivalent to
	 * the Java {@code %} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a%b}
	 */
	@Pure
	@Inline(value = "(#I1 % #I2)", constantExpression=true)
	public static #RT operator_modulo(#T1 a, #T2 b) {
		return #A % #B;
	}
',
	'operator_lessThan' => '
	/** The binary {@code lessThan} operator. This is the equivalent to
	 * the Java {@code &lt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;b}
	 */
	@Pure
	@Inline(value = "(#I1 < #I2)", constantExpression=true)
	public static boolean operator_lessThan(#T1 a, #T2 b) {
		return #A < #B;
	}
',
	'operator_lessEqualsThan' => '
	/** The binary {@code lessEqualsThan} operator. This is the equivalent
	 * to the Java {@code &lt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&lt;=b}
	 */
	@Pure
	@Inline(value = "(#I1 <= #I2)", constantExpression=true)
	public static boolean operator_lessEqualsThan(#T1 a, #T2 b) {
		return #A <= #B;
	}
',
	'operator_greaterThan' => '
	/** The binary {@code greaterThan} operator. This is the equivalent
	 * to the Java {@code &gt;} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;b}
	 */
	@Pure
	@Inline(value = "(#I1 > #I2)", constantExpression=true)
	public static boolean operator_greaterThan(#T1 a, #T2 b) {
		return #A > #B;
	}

',
	'operator_greaterEqualsThan' => '
	/** The binary {@code greaterEqualsThan} operator. This is the equivalent
	 * to the Java {@code &gt;=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a&gt;=b}
	 */
	@Pure
	@Inline(value = "(#I1 >= #I2)", constantExpression=true)
	public static boolean operator_greaterEqualsThan(#T1 a, #T2 b) {
		return #A >= #B;
	}
',
	'operator_equals' => '
	/**
	 *The binary {@code equals} operator. This is the equivalent to the Java {@code ==} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a==b}
	 */
	@Pure
	@Inline(value = "(#I1 == #I2)", constantExpression=true)
	public static boolean operator_equals(#T1 a, #T2 b) {
		return #A == #B;
	}
',
	'operator_notEquals' => '
	/** The binary {@code notEquals} operator. This is the equivalent
	 * to the Java {@code !=} operator.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code a!=b}
	 */
	@Pure
	@Inline(value = "(#I1 != #I2)", constantExpression=true)
	public static boolean operator_notEquals(#T1 a, #T2 b) {
		return #A != #B;
	}
',
	'operator_power' => '
	/** The binary {@code power} operator. This is the equivalent to
	 * the Java\'s {@code Math.pow()} function.
	 *
	 * @param a a number.
	 * @param b a number.
	 * @return {@code Math::pow(a, b)}
	 */
	@Pure
	@Inline(value = "$3.pow(#I1, #I2)", imported = Math.class)
	public static double operator_power(#T1 a, #T2 b) {
		return Math.pow(#A, #B);
	}
',
);

my %PRIMITIVES = (
	'double' => 1,
	'float' => 1,
	'long' => 1,
	'int' => 1,
	'short' => 1,
	'byte' => 1,
);

my %OBJECTS = (
	'Double' => 1,
	'Float' => 1,
	'Long' => 1,
	'Integer' => 1,
	'Short' => 1,
	'Byte' => 1,
);

my %ATOMICS = (
	'AtomicLong' => 1,
	'AtomicInteger' => 1,
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
	'.doubleValue()',
	'.floatValue()',
	'.longValue()',
	'.intValue()',
	'.shortValue()',
	'.byteValue()',
);

my @RETURN_TYPES = (
	'double',
	'float',
	'long',
	'int',
	'int',
	'int',
);

sub acc($$) {
	if ($PRIMITIVES{$_[0]}) {
		return '';
	}
	return $ACCESSORS[$_[1] - 1];
}

sub apply1($$$$) {
	my $r = "$_[0]";
	my $rt = "$_[1]";
	my $left = "$_[2]";
	my $ucleft = ucfirst("$left");
	my $acc1 = "$_[3]";
	$r =~ s/\Q#RT\E/$rt/g;
	$r =~ s/\Q#T\E/$left/g;
	$r =~ s/\Q#UCT\E/$ucleft/g;
	$r =~ s/\Q#A\E/i$acc1/g;
	my $i1 = ($acc1) ? "(\$1)$acc1" : "\$1";
	$r =~ s/\Q#I\E/$i1/g;
	return $r;
}

sub apply2($$$$$$) {
	my $r = "$_[0]";
	my $rt = "$_[1]";
	my $left = "$_[2]";
	my $right = "$_[3]";
	my $ucleft = ucfirst("$left");
	my $ucright = ucfirst("$right");
	my $acc1 = "$_[4]";
	my $acc2 = "$_[5]";
	$r =~ s/\Q#RT\E/$rt/g;
	$r =~ s/\Q#T1\E/$left/g;
	$r =~ s/\Q#T2\E/$right/g;
	$r =~ s/\Q#UCT1\E/$ucleft/g;
	$r =~ s/\Q#UCT2\E/$ucright/g;
	$r =~ s/\Q#A\E/a$acc1/g;
	$r =~ s/\Q#B\E/b$acc2/g;
	my $i1 = ($acc1) ? "(\$1)$acc1" : "\$1";
	$r =~ s/\Q#I1\E/$i1/g;
	my $i2 = ($acc2) ? "(\$2)$acc2" : "\$2";
	$r =~ s/\Q#I2\E/$i2/g;
	return $r;
}

while (my $file = glob("*OperatorExtensions.java")) {
	unlink($file) or die("$file: $!\n");
}

my @FILES = ();

for my $left (keys(%PRIMITIVES), keys(%OBJECTS), keys(%ATOMICS)) {
	local *FILE;
	my $basename = (($PRIMITIVES{$left}) ? "Primitive" . ucfirst($left) : ${left}). "OperatorExtensions";
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

/** Provide static operators for numbers of type {\@code $left}.
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

	while (my ($key, $res) = each(%MONO_TEMPLATES)) {
		if (!$MONO_ERRORS{$key}{$left}) {
			my $levelLeft = $LEVEL{$left};
			my $rt = $RETURN_TYPES[$levelLeft - 1];
			my $acc1 = acc($left, $levelLeft);
			my $r = apply1("$res", $rt, $left, $acc1);
			print FILE "$r";
		}
	}

	while (my ($key, $res) = each(%BI_TEMPLATES)) {
		for my $right (keys(%PRIMITIVES), keys(%OBJECTS), keys(%ATOMICS)) {
			if ((!$BI_ERRORS{$key}{$left}{$right}) &&
				(!$PRIMITIVES{$left} || !$PRIMITIVES{$right})) {
				my $levelLeft = $LEVEL{$left};
				my $levelRight = $LEVEL{$right};
				my $maxLevel = ($levelLeft < $levelRight) ? $levelLeft : $levelRight;
				my $rt = $RETURN_TYPES[$maxLevel - 1];
				my $acc1 = acc($left, $maxLevel);
				my $acc2 = acc($right, $maxLevel);
				my $r = apply2("$res", $rt, $left, $right, $acc1, $acc2);
				print FILE "$r";
			}
		}
	}

	print FILE "\n\t// END GENERATED BLOCK\n\n}\n";
	close(*FILE);
}

local *FILE;
my $basename = "NumberOperatorExtensions";
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

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static operators for numbers of type {\@code Number}.
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
while (my ($key, $res) = each(%BI_TEMPLATES)) {
	if (!$BI_ERRORS{$key}{'Number'}{'Number'}) {
		my $r = apply2("$res", "double", "Number", "Number", ".doubleValue()", ".doubleValue()");
		print FILE "$r";
	}
}
print FILE "\n\t// END GENERATED BLOCK\n\n}\n";
close(*FILE);

for my $file (@FILES) {
	print "\t\tfeatures.add($file.class);\n";
}

