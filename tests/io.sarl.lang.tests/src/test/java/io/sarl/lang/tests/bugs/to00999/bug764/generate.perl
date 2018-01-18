#!/usr/bin/env perl

use strict;
use Data::Dumper;

my %MONO_ERRORS = (
);
my %BI_ERRORS = (
);

my %MONO_TEMPLATES = (
	'operator_minus' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T) : #RT {",
		"      -a",
		"   }",
		"}");
',
);

my %BI_TEMPLATES = (
	'operator_plus' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : #RT {",
		"      a + b",
		"   }",
		"}");
',
	'operator_minus' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : #RT {",
		"      a - b",
		"   }",
		"}");
',
	'operator_multiply' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : #RT {",
		"      a * b",
		"   }",
		"}");
',
	'operator_divide' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : #RT {",
		"      a / b",
		"   }",
		"}");
',
	'operator_modulo' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : #RT {",
		"      a % b",
		"   }",
		"}");
',
	'operator_lessThan' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : boolean {",
		"      a < b",
		"   }",
		"}");
',
	'operator_lessEqualsThan' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : boolean {",
		"      a <= b",
		"   }",
		"}");
',
	'operator_greaterThan' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : boolean {",
		"      a > b",
		"   }",
		"}");
',
	'operator_greaterEqualsThan' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : boolean {",
		"      a >= b",
		"   }",
		"}");
',
	'operator_equals' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : boolean {",
		"      a == b",
		"   }",
		"}");
',
	'operator_notEquals' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : boolean {",
		"      a != b",
		"   }",
		"}");
',
	'operator_power' => '
	private String SNIPSET_#N = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : #T1, b : #T2) : double {",
		"      a ** b",
		"   }",
		"}");
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

my $N = 0;

sub acc($$) {
	if ($PRIMITIVES{$_[0]}) {
		return '';
	}
	return $ACCESSORS[$_[1] - 1];
}

sub apply1($$$$$) {
	my $r = "$_[0]";
	my $rt = "$_[1]";
	my $left = "$_[2]";
	my $acc1 = "$_[3]";
	my $iserror = "$_[4]";
	$r =~ s/\Q#RT\E/$rt/g;
	$r =~ s/\Q#T\E/$left/g;
	$r =~ s/\Q#A\E/i$acc1/g;
	my $i1 = ($acc1) ? "(\$1)$acc1" : "\$1";
	$r =~ s/\Q#I\E/$i1/g;
	if ($iserror) {
		$r .= '
	@Test
	public void parsing_#N() throws Exception {
		validate(file(this.SNIPSET_#N)).assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.AMBIGUOUS_FEATURE_CALL);
	}
';
	} else {
		$r .= '
	@Test
	public void parsing_#N() throws Exception {
		validate(file(this.SNIPSET_#N)).assertNoErrors();
	}
';
	}
	$r =~ s/\Q#N\E/$N/g;
	$N++;
	return $r;
}

sub apply2($$$$$$$) {
	my $r = "$_[0]";
	my $rt = "$_[1]";
	my $left = "$_[2]";
	my $right = "$_[3]";
	my $acc1 = "$_[4]";
	my $acc2 = "$_[5]";
	my $iserror = "$_[6]";
	$r =~ s/\Q#RT\E/$rt/g;
	$r =~ s/\Q#T1\E/$left/g;
	$r =~ s/\Q#T2\E/$right/g;
	$r =~ s/\Q#A\E/a$acc1/g;
	$r =~ s/\Q#B\E/b$acc2/g;
	my $i1 = ($acc1) ? "(\$1)$acc1" : "\$1";
	$r =~ s/\Q#I1\E/$i1/g;
	my $i2 = ($acc2) ? "(\$2)$acc2" : "\$2";
	$r =~ s/\Q#I2\E/$i2/g;
	if ($iserror) {
		$r .= '
	@Test
	public void parsing_#N() throws Exception {
		validate(file(this.SNIPSET_#N)).assertError(
				XbasePackage.eINSTANCE.getXBinaryOperation(),
				IssueCodes.AMBIGUOUS_FEATURE_CALL);
	}
';
	} else {
		$r .= '
	@Test
	public void parsing_#N() throws Exception {
		validate(file(this.SNIPSET_#N)).assertNoErrors();
	}
';
	}
	$r =~ s/\Q#N\E/$N/g;
	$N++;
	return $r;
}

for my $left (keys(%PRIMITIVES), keys(%OBJECTS), keys(%ATOMICS)) {
	local *FILE;
	my $basename = (($PRIMITIVES{$left}) ? "Primitive" . ucfirst($left) : ${left}). "ExtensionsTest";
	open(*FILE, "> ${basename}.java") or die("$!\n");
	print FILE "/*
/*
 * Copyright (C) 2014-2018 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the \"License\");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an \"AS IS\" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.tests.bugs.to00999.bug764;

import static org.junit.Assert.*;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import com.google.common.base.Objects;
import com.google.inject.Inject;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class.
 *
 * \@author \$Author: sgalland\$
 * \@version \$Name\$ \$Revision\$ \$Date\$
 * \@mavengroupid \$GroupId\$
 * \@mavenartifactid \$ArtifactId\$
 * \@see \"https://github.com/sarl/sarl/issues/764\"
 */
\@SuppressWarnings(\"all\")
public class ${basename} extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK\n";

	while (my ($key, $res) = each(%MONO_TEMPLATES)) {
		my $levelLeft = $LEVEL{$left};
		my $rt = $RETURN_TYPES[$levelLeft - 1];
		my $acc1 = acc($left, $levelLeft);
		my $r = apply1("$res", $rt, $left, $acc1, $MONO_ERRORS{$key}{$left});
		print FILE "$r";
	}

	while (my ($key, $res) = each(%BI_TEMPLATES)) {
		for my $right (keys(%PRIMITIVES), keys(%OBJECTS), keys(%ATOMICS)) {
			my $levelLeft = $LEVEL{$left};
			my $levelRight = $LEVEL{$right};
			my $maxLevel = ($levelLeft < $levelRight) ? $levelLeft : $levelRight;
			my $rt = $RETURN_TYPES[$maxLevel - 1];
			my $acc1 = acc($left, $maxLevel);
			my $acc2 = acc($right, $maxLevel);
			my $r = apply2("$res", $rt, $left, $right, $acc1, $acc2, $BI_ERRORS{$key}{$left}{$right});
			print FILE "$r";
		}
	}

	print FILE "\n\t// END GENERATED BLOCK\n\n}\n";
	close(*FILE);
}

local *FILE;
my $basename = "NumberExtensionsTest";
open(*FILE, "> ${basename}.java") or die("$!\n");
print FILE "/*
/*
 * Copyright (C) 2014-2018 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the \"License\");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an \"AS IS\" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.tests.bugs.to00999.bug764;

import static org.junit.Assert.*;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import com.google.common.base.Objects;
import com.google.inject.Inject;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class.
 *
 * \@author \$Author: sgalland\$
 * \@version \$Name\$ \$Revision\$ \$Date\$
 * \@mavengroupid \$GroupId\$
 * \@mavenartifactid \$ArtifactId\$
 * \@see \"https://github.com/sarl/sarl/issues/764\"
 */
\@SuppressWarnings(\"all\")
public class ${basename} extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK\n";

while (my ($key, $res) = each(%BI_TEMPLATES)) {
	my $r = apply2("$res", "double", "Number", "Number", ".doubleValue()", ".doubleValue()", $BI_ERRORS{$key}{'Number'}{'Number'});
	print FILE "$r";
}

print FILE "\n\t// END GENERATED BLOCK\n\n}\n";
close(*FILE);


