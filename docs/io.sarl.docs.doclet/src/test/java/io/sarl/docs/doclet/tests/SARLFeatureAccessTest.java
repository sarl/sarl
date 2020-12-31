/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.docs.doclet.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.google.inject.Injector;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.docs.doclet.utils.SARLFeatureAccess;
import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Fake accessor to SARL keywords.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
@SuppressWarnings("all")
@DisplayName("SARLFeatureAccess")
@Tag("javadoc")
@Tag("unit")
public final class SARLFeatureAccessTest {

	private io.sarl.lang.services.SARLGrammarKeywordAccess original;

	private SARLFeatureAccess access;
	
	@BeforeEach
	public void setUp() {
		Injector injector = SARLStandaloneSetup.doSetup();
		this.original = injector.getInstance(SARLGrammarKeywordAccess.class);
		this.access = new SARLFeatureAccess();
	}

	@Test
	public void testSARL_AGENT() {
		assertEquals(SarlPackage.SARL_AGENT, SARLFeatureAccess.SARL_AGENT);
	}

	@Test
	public void testSARL_EVENT() {
		assertEquals(SarlPackage.SARL_EVENT, SARLFeatureAccess.SARL_EVENT);
	}

	@Test
	public void testSARL_BEHAVIOR() {
		assertEquals(SarlPackage.SARL_BEHAVIOR, SARLFeatureAccess.SARL_BEHAVIOR);
	}

	@Test
	public void testSARL_CAPACITY() {
		assertEquals(SarlPackage.SARL_CAPACITY, SARLFeatureAccess.SARL_CAPACITY);
	}

	@Test
	public void testSARL_SKILL() {
		assertEquals(SarlPackage.SARL_SKILL, SARLFeatureAccess.SARL_SKILL);
	}

	@Test
	public void testSARL_SPACE() {
		assertEquals(SarlPackage.SARL_SPACE, SARLFeatureAccess.SARL_SPACE);
	}

	@Test
	public void testSARL_ARTIFACT() {
		assertEquals(SarlPackage.SARL_ARTIFACT, SARLFeatureAccess.SARL_ARTIFACT);
	}
	
	@Test
	public void testDef() {
		assertEquals(original.getDefKeyword(), access.getDefKeyword());
	}

	@Test
	public void testColon() {
		assertEquals(original.getColonKeyword(), access.getColonKeyword());
	}

	@Test
	public void testWith() {
		assertEquals(original.getWithKeyword(), access.getWithKeyword());
	}

	@Test
	public void testEquals() {
		assertEquals(original.getEqualsSignKeyword(), access.getEqualsSignKeyword());
	}

	@Test
	public void testComma() {
		assertEquals(original.getCommaKeyword(), access.getCommaKeyword());
	}

	@Test
	public void testExtends() {
		assertEquals(original.getExtendsKeyword(), access.getExtendsKeyword());
	}

	@Test
	public void testSuper() {
		assertEquals(original.getSuperKeyword(), access.getSuperKeyword());
	}

	@Test
	public void testAmpersand() {
		assertEquals(original.getAmpersandKeyword(), access.getAmpersandKeyword());
	}

	@Test
	public void testSquareBrackets() {
		assertEquals(original.getLeftSquareBracketKeyword() + original.getRightSquareBracketKeyword(), access.getSquareBracketKeywords());
	}

	@Test
	public void testWildcardAsterisk() {
		assertEquals(original.getWildcardAsteriskKeyword(), access.getWildcardAsteriskKeyword());
	}

	@Test
	public void testLeftParenthesis() {
		assertEquals(original.getLeftParenthesisKeyword(), access.getLeftParenthesisKeyword());
	}

	@Test
	public void testRightParenthesis() {
		assertEquals(original.getRightParenthesisKeyword(), access.getRightParenthesisKeyword());
	}

	@Test
	public void testEqualsGreaterThan() {
		assertEquals(original.getEqualsSignGreaterThanSignKeyword(), access.getEqualsSignGreaterThanSignKeyword());
	}

	@Test
	public void testVoid() {
		assertEquals(original.getVoidKeyword(), access.getVoidKeyword());
	}

	@Test
	public void testVal() {
		assertEquals(original.getValKeyword(), access.getValKeyword());
	}

	@Test
	public void testVar() {
		assertEquals(original.getWriteableVarKeyword(), access.getVarKeyword());
	}

	@Test
	public void testNew() {
		assertEquals(original.getNewKeyword(), access.getNewKeyword());
	}

	@Test
	public void getSarlElementTypeAnnotationName() {
		assertEquals(SarlElementType.class.getName(), access.getSarlElementTypeAnnotationName());
	}

	@Test
	public void getDefaultValueAnnnotationName() {
		assertEquals(DefaultValue.class.getName(), access.getDefaultValueAnnnotationName());
	}

	@Test
	public void getSarlSourceCodeAnnotationName() {
		assertEquals(SarlSourceCode.class.getName(), access.getSarlSourceCodeAnnotationName());
	}

	@Test
	public void getPureAnnotationName() {
		assertEquals(Pure.class.getName(), access.getPureAnnotationName());
	}

	@Test
	public void getSyntheticMemberAnnotationName() {
		assertEquals(SyntheticMember.class.getName(), access.getSyntheticMemberAnnotationName());
	}

	@Test
	public void getProceduresName() {
		assertEquals(Procedures.class.getName(), access.getProceduresName());
	}

	@Test
	public void getFunctionsName() {
		assertEquals(Functions.class.getName(), access.getFunctionsName());
	}

}
