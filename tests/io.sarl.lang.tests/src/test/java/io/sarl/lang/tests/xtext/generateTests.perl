#!/usr/bin/env perl
# Copyright (c) 2018 Universite Bourgogne Franche-Comté (http://www.ubfc.fr)
#                    Universite de Technologie de Belfort-Montbeliard (http://www.utbm.fr), and others.
# All rights reserved. This program and the accompanying
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html

use strict;
use Data::Dumper;
use File::Path;
use IO::Handle;

use constant USE_SARL => 1;

my @IGNORABLE_CODES;

if (USE_SARL) {
	@IGNORABLE_CODES = (
		#"io.sarl.lang.validation.IssueCodes.redundant_interface_implementation",
	);
} else {
	@IGNORABLE_CODES = (
	);
}

sub fctproto($$@) {
	my $name = shift;
	my $rettype = shift;
	my $p;

	if (USE_SARL) {
		# SARL syntax
		$p = "def $name(";
		my $i = 0; 
		while ($i < @_) {
			if ($i > 0) {
				$p .= ", ";
			}
			my $ptype = $_[$i];
			my $pname = $_[$i+1];
			$i += 2;
			$p .= "$pname : $ptype";
		}
		$p .= ") : $rettype";
	} else {
		# Xtend syntax
		my $p = "def $rettype $name(";
		my $i = 0; 
		while ($i < @_) {
			if ($i > 0) {
				$p .= ", ";
			}
			my $ptype = $_[$i];
			my $pname = $_[$i+1];
			$i += 2;
			$p .= "$ptype : $pname";
		}
		$p .= ")";
	}

	return $p;
}

sub createInterfaces($$@) {
	my $t = shift;
	my $name = shift;
	my @ss = (@_);
	push @ss, '';
	for my $super (@ss) {
		my $extends = '';
		if ($super) {
			$extends = " extends $super";
		}
		$t->{$name} = () unless ($t->{$name});
		push @{$t->{$name}},
			{ 'java' =>
				 "\t\tpublic interface $name$extends {\n"
				."\t\t  public void fct(int arg);\n"
				."\t\t}",
			  'dsl' =>
				"\t\t\t\tpublic interface $name$extends {\n"
				."\t\t\t\t  public ".fctproto("fct", "void", "int", "arg")."\n"
				."\t\t\t\t}"
			};

		push @{$t->{$name}},
			{ 'java' =>
				 "\t\tpublic interface $name$extends {\n"
				."\t\t  public default void fct(int arg) {}\n"
				."\t\t}",
			  'dsl' =>
				 "\t\t\t\tpublic interface $name$extends {\n"
				."\t\t\t\t  public ".fctproto("fct", "void", "int", "arg")." {}\n"
				."\t\t\t\t}"
			};

		push @{$t->{$name}},
			{ 'java' =>
				 "\t\tpublic interface $name$extends {\n"
				."\t\t}",
			  'dsl' =>
				 "\t\t\t\tpublic interface $name$extends {\n"
				."\t\t\t\t}"
			};
	}
}

sub createClasses($$$$$@) {
	my $t = shift;
	my $abs = shift;
	my $rel = shift;
	my $name = shift;
	my $superclass = shift;
	my @ss = ();
	if ($rel eq 'and') {
		@ss = (join(', ', @_), $_[0]);
	} else {
		@ss = (@_);
		push @ss, '';
	}
	my $extends = '';
	if ($superclass) {
		$extends = " extends $superclass";
	}
	if ($abs) {
		$abs = " abstract";
	} else {
		$abs = '';
	}
	for my $super (@ss) {
		my $impls = '';
		if ($super) {
			$impls = " implements $super";
		}
		$t->{$name} = () unless ($t->{$name});

		push @{$t->{$name}},
			{ 'java' =>
				 "\t\tpublic$abs static class $name$extends$impls {\n"
				."\t\t  public void fct(int arg) {}\n"
				."\t\t}",
			  'dsl' =>
				 "\t\t\t\tpublic$abs class $name$extends$impls {\n"
				."\t\t\t\t  public ".fctproto("fct", "void", "int", "arg")." {}\n"
				."\t\t\t\t}"
			};

		push @{$t->{$name}},
			{ 'java' =>
				 "\t\tpublic$abs static class $name$extends$impls {\n"
				."\t\t  public abstract void fct(int arg);\n"
				."\t\t}",
			  'dsl' => 
				 "\t\t\t\tpublic$abs class $name$extends$impls {\n"
				."\t\t\t\t  public abstract ".fctproto("fct", "void", "int", "arg")."\n"
				."\t\t\t\t}"
			};

		push @{$t->{$name}},
			{ 'java' =>
				 "\t\tpublic$abs static class $name$extends$impls {\n"
				."\t\t}",
			  'dsl' =>
				 "\t\t\t\tpublic$abs class $name$extends$impls {\n"
				."\t\t}",
			};
	}
}

sub createClassesAnd($$$$@) {
	my $t = shift;
	my $abs = shift;
	my $name = shift;
	my $superclass = shift;
	createClasses($t, $abs, 'and', $name, $superclass, @_);
}

sub createClassesOr($$$$@) {
	my $t = shift;
	my $abs = shift;
	my $name = shift;
	my $superclass = shift;
	createClasses($t, $abs, 'or', $name, $superclass, @_);
}

sub generateCases(\%) {
	my $t = shift;
	my @cases = ();
	for my $k (keys(%{$t})) {
		if (@cases) {
			my @ncases = ();
			for my $prefix (@cases) {
				for my $codes (@{$t->{$k}}) {
					push @ncases, [(@{$prefix}, $codes)];
				}
			}
			@cases = @ncases;
		} else {
			for my $codes (@{$t->{$k}}) {
				push @cases, [$codes];
			}
		}
	}
	return @cases;
}

sub generateJavaCode(\@) {
	my $java =
		 "package mypack;\n"
		."\@SuppressWarnings(\"all\")\n"
		."public class SystematicInheritanceTest {\n\n";

	for my $elt (@{$_[0]}) {
		$java .= $elt."\n";
	}

	$java .= "\n}";
	return $java;
}

sub generateDSLCode($$@) {
	my $ok = shift;
	my $i = shift;
	my $dsl = '';
	$dsl .= "public class SystematicInheritanceTest$i extends ";
	if (USE_SARL) {
		$dsl .= "AbstractSarlTest";
	} else {
		$dsl .= "AbstractXtendTestCase";
	}
	$dsl .= " {\n";
	$dsl .= "\n";
	$dsl .= "\t\@Inject private ValidationTestHelper helper;\n";
	$dsl .= "\n";
	if (!USE_SARL) {
		$dsl .= "\tprivate static String multilineString(String... lines) {\n";
		$dsl .= "\t\tStringBuilder b = new StringBuilder();\n";
		$dsl .= "\t\tboolean first = true;\n";
		$dsl .= "\t\tfor (String line : lines) {\n";
		$dsl .= "\t\t\tif (first) {\n";
		$dsl .= "\t\t\t\tfirst = false;\n";
		$dsl .= "\t\t\t} else {\n";
		$dsl .= "\t\t\t\tb.append(\"\\n\");\n";
		$dsl .= "\t\t\t}\n";
		$dsl .= "\t\t\tb.append(line);\n";
		$dsl .= "\t\t}\n";
		$dsl .= "\t\treturn b.toString();\n";
		$dsl .= "\t}\n";
		$dsl .= "\n";
		if (!$ok) {
			$dsl .= "\tpublic void assertAnyError(EObject source) {\n";
			$dsl .= "\t\tfinal List<Issue> validate = this.validationHelper.validate(source);\n";
			$dsl .= "\t\tif (!any(validate, input -> Severity.ERROR == input.getSeverity())) {\n";
			$dsl .= "\t\t\tfail(\"Expected an error, but got nothing\");\n";
			$dsl .= "\t\t}\n";
			$dsl .= "\t}\n";
		} else {
			$dsl .= "\tpublic void assertNoErrorsExcept(EObject source, String... codes) {\n";
			$dsl .= "\t\tfinal List<String> codeSet = Arrays.asList(codes);\n";
			$dsl .= "\t\tfinal List<Issue> validate = this.validationHelper.get().validate(source);\n";
			$dsl .= "\t\tif (any(validate, input -> Severity.ERROR == input.getSeverity() && !codeSet.contains(input.getCode()))) {\n";
			$dsl .= "\t\t\tfail(\"Expected no error, found: \" + validate);\n";
			$dsl .= "\t\t}\n";
			$dsl .= "\t}\n";
		}
		$dsl .= "\n";
	} else {
		$dsl .= "\t\@Before\n";
		$dsl .= "\tpublic void setUp() throws Exception{\n";
		$dsl .= "\t\tassumeTrue(Objects.equals(System.getenv(\"io.sarl.lang.tests.optionalTests\"), \"true\"));\n";
		$dsl .= "\t}\n";
		$dsl .= "\n";
	}
	$dsl .= "\t\@Test\n";
	$dsl .= "\tpublic void test() throws Exception {\n";
	$dsl .= "\t\t";
	if ($ok) {
		if (@IGNORABLE_CODES) {
			$dsl .= "assertNoErrorsExcept";
		} else {
			$dsl .= "this.helper.assertNoErrors";
		}
	} else {
		$dsl .= "assertAnyError";
	}
	$dsl .= "(file(multilineString(\n";
	my $first = 1;
	for my $elt (@_) {
		my $formatted = $elt;
		$formatted =~ s/\s+$//s;
		$formatted =~ s/^\s+//s;
		$formatted =~ s/\n+\t*/",\n\t\t\t"/sg;
		if ($first) {
			$first = 0;
		} else {
			$dsl .= ",\n";
		}
		$dsl .= "\t\t\t\"$formatted\"";
	}
	$dsl .= "))";
	if ($ok && @IGNORABLE_CODES) {
		foreach my $myign (@IGNORABLE_CODES) {
			$dsl .= ",\n\t\t\t\"$myign\"";
		}
	}
	$dsl .= ");\n\t}\n";
	$dsl .= "}\n";
	return $dsl;
}

sub formatJavaDoc($) {
	my $code = shift;
	my @ret = ();
	foreach my $line (split(/[\n\r]/, $code)) {
		push @ret, " * \t$line\n";
	}
	my $ret = join('', @ret);
	$ret =~ s/[\n\r \t]+$//s;
	$ret =~ s/^[\n\r]+//s;
	return "$ret";
}

sub extract($$) {
	my $array = shift;
	my $key = shift;
	my @ret = ();
	foreach my $elt (@{$array}) {
		push @ret, $elt->{$key};
	}
	return @ret;
}

sub doJavaCompilation($) {
	my $java = shift;
	rmtree("./java");
	mkpath("./java/mypack") or die("mkpath: $!\n");
	local *FILE;
	open(*FILE, "> ./java/mypack/SystematicInheritanceTest.java") or die("SystematicInheritanceTest.java: $!\n");
	print FILE ($java);
	close(*FILE);
	chdir("./java") or die("chdir: $!\n");
	my $err = `/usr/bin/javac -nowarn mypack/SystematicInheritanceTest.java 2>&1`;
	my $ok = ($? == 0);
	$err =~ s/^[\n\r\t ]+//s;
	$err =~ s/[\n\r\t ]+$//s;
	if (!$err) {
		$err = "no java error";
	}
	chdir("..") or die("chdir2: $!\n");
	rmtree("./java") or die("rmtree: $!\n");
	return ($ok, $err);
}

my %types = ();
createInterfaces(\%types, 'I1', 'I2');
createInterfaces(\%types, 'I2');
createInterfaces(\%types, 'I3', 'I1', 'I2');

createClassesOr(\%types, 1, 'IC', '', 'I1', 'I2');
createClassesAnd(\%types, 0, 'C', 'IC', 'I1', 'I3');

my @allCases = generateCases(%types);

rmtree("./systematicInheritanceTests");
mkpath("./systematicInheritanceTests") or die("path: $!\n");

my $max = @allCases;
my $i = 0;

## DEBUG
#$max = 15;

while ($i < $max) {
	my @jcase = extract($allCases[$i], 'java');
	my @dcase = extract($allCases[$i], 'dsl');
	my $java = generateJavaCode(@jcase);
	my $formattedJava = formatJavaDoc($java);	
	print "".($i+1)."/$max\n";
	# Java Compilation
	my ($ok, $err) = doJavaCompilation($java);
	$err = formatJavaDoc($err);
	# Create the unit test
	my $out = "./systematicInheritanceTests/SystematicInheritanceTest$i.java";

	local *FILE;
	open(*FILE, "> ./systematicInheritanceTests/SystematicInheritanceTest$i.java") or die("SystematicInheritanceTest$i.java: $!\n");
	print FILE (
		 "/**\n"
		." * Copyright (c) 2018 Universite Bourgogne Franche-Comté (http://www.ubfc.fr)\n"
		." *                    Universite de Technologie de Belfort-Montbeliard (http://www.utbm.fr), and others.\n"
		." * All rights reserved. This program and the accompanying materials\n"
		." * are made available under the terms of the Eclipse Public License v1.0\n"
		." * which accompanies this distribution, and is available at\n"
		." * http://www.eclipse.org/legal/epl-v10.html\n"
		." */\n");
	if (USE_SARL) {
		print FILE (
			 "package io.sarl.lang.tests.xtext.systematicInheritanceTests;\n"
			."\n"
			."import java.util.Objects;\n"
			."import com.google.inject.Inject;\n"
			."import org.eclipse.xtext.diagnostics.Severity;\n"
			."import org.eclipse.xtext.testing.InjectWith;\n"
			."import org.eclipse.xtext.testing.validation.ValidationTestHelper;\n"
			."import org.junit.Before;\n"
			."import org.junit.Test;\n"
			."\n"
			."import io.sarl.tests.api.AbstractSarlTest;\n"
			."\n"
			."import static org.junit.Assert.*;\n"
			."import static org.junit.Assume.*;\n"
			."import static org.eclipse.xtend.core.validation.IssueCodes.*;\n"
			."import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.*;\n"
			."import static org.eclipse.xtext.xbase.XbasePackage.Literals.*;\n"
			."import static org.eclipse.xtext.xbase.validation.IssueCodes.*;\n");
	} else {
		print FILE (
			 "package org.eclipse.xtext.xbase.tests.typesystem.inheritance.systematicInheritanceTests;\n"
			."\n"
			."import java.util.List;\n"
			."import java.util.Arrays;\n"
			."\n"
			."import com.google.inject.Inject;\n"
			."import org.eclipse.xtend.core.tests.AbstractXtendTestCase;\n"
			."import org.eclipse.xtend.core.tests.java8.Java8RuntimeInjectorProvider;\n"
			."import org.eclipse.xtext.diagnostics.Severity;\n"
			."import org.eclipse.xtext.testing.InjectWith;\n"
			."import org.eclipse.xtext.validation.Issue;\n"
			."import org.eclipse.xtext.testing.validation.ValidationTestHelper;\n"
			."import org.junit.Test;\n"
			."\n"
			."import static org.junit.Assert.*;\n"
			."import static org.junit.Assume.*;\n"
			."import static org.eclipse.xtend.core.validation.IssueCodes.*;\n"
			."import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.*;\n"
			."import static org.eclipse.xtext.xbase.XbasePackage.Literals.*;\n"
			."import static org.eclipse.xtext.xbase.validation.IssueCodes.*;\n");
	}
	print FILE (
			 "\n"
			."/** This unit test is automatically generated by the {\@code generateTests.pl} script.\n"
			." * DO NOT CHANGE THE CODE OF THIS UNIT TEST.\n"
			." *\n"
			." * <p>Java code:\n"
			." * <pre><code>\n$formattedJava\n"
			." * </code></pre>\n"
			." *\n"
			." * <p>javac output:\n"
			." * <pre><code>\n$err\n"
			." * </code></pre>\n"
			." * \@author Stephane Galland - Initial contribution and API\n");
	if (USE_SARL) {
		print FILE (
			 " * \@see \"https://github.com/eclipse/xtext-xtend/pull/192\"\n");
	}

	print FILE (
			 " */\n"
			."\@SuppressWarnings(\"all\")\n");

	if (!USE_SARL) {
		print FILE (
			 "\@InjectWith(Java8RuntimeInjectorProvider.class)\n");
	}

	my $dsl = generateDSLCode($ok, $i, @dcase);
	print FILE ($dsl);
	close(*FILE);
	$i++;

}

