/*
 * Copyright 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.bugs.to00399;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/** Test for the issue: Manage the mutual exclusion of agent attributes
 * used in different "on" statements or in different behaviors.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/208"
 */
@RunWith(Suite.class)
@SuiteClasses({
	Bug208.WithProblem.class,
	Bug208.WithoutProblem.class,
})
@SuppressWarnings("all")
public class Bug208 {
	
	public static class WithProblem extends AbstractSarlTest {
		
		private static final String SOURCE_04 = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  def fct {",
				"    this.field = 2",
				"  }",
				"}");

		@Test
		public void testParser04() throws Exception {
			SarlScript script = file(SOURCE_04);
			validate(script).assertWarning(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_05 = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  def fct {",
				"    this.field = 2",
				"  }",
				"  def fct2 : int {",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser05() throws Exception {
			SarlScript script = file(SOURCE_05);
			validate(script).assertWarning(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_06 = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private def fct {",
				"    this.field = 2",
				"  }",
				"  private def fct2 : int {",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser06() throws Exception {
			SarlScript script = file(SOURCE_06);
			validate(script).assertWarning(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_07a = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private def fct : int {",
				"    this.field = 2",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser07a() throws Exception {
			SarlScript script = file(SOURCE_07a);
			validate(script).assertWarning(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_07b = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private def fct : int {",
				"    synchronized(this) {this.field = 2}",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser07b() throws Exception {
			SarlScript script = file(SOURCE_07b);
			validate(script).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_07c = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private def fct : int {",
				"    this.field = 2",
				"    synchronized(this) {return this.field}",
				"  }",
				"}");

		@Test
		public void testParser07c() throws Exception {
			SarlScript script = file(SOURCE_07c);
			validate(script).assertWarning(
					XbasePackage.eINSTANCE.getXAssignment(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

	}

	public static class WithoutProblem extends AbstractSarlTest {
		
		private static final String SOURCE_01 = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  val field = 1",
				"}");

		@Test
		public void testParser01() throws Exception {
			SarlScript script = file(SOURCE_01);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_02 = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  val field : int",
				"  new {",
				"    super(null, null)",
				"    this.field = 1",
				"  }",
				"  def fct : int {",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser02() throws Exception {
			SarlScript script = file(SOURCE_02);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_03 = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  val field = 1",
				"  def fct : int {",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser03() throws Exception {
			SarlScript script = file(SOURCE_03);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_04a = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  synchronized def fct {",
				"    this.field = 2",
				"  }",
				"}");

		@Test
		public void testParser04a() throws Exception {
			SarlScript script = file(SOURCE_04a);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_04b = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  def fct {",
				"    synchronized(this) {this.field = 2}",
				"  }",
				"}");

		@Test
		public void testParser04b() throws Exception {
			SarlScript script = file(SOURCE_04b);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_05a = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  synchronized def fct {",
				"    this.field = 2",
				"  }",
				"  synchronized def fct2 : int {",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser05a() throws Exception {
			SarlScript script = file(SOURCE_05a);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_05b = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  def fct {",
				"    synchronized (this) {this.field = 2}",
				"  }",
				"  synchronized def fct2 : int {",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser05b() throws Exception {
			SarlScript script = file(SOURCE_05b);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_05c = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  synchronized def fct {",
				"    this.field = 2",
				"  }",
				"  def fct2 : int {",
				"    synchronized (this) {return this.field}",
				"  }",
				"}");

		@Test
		public void testParser05c() throws Exception {
			SarlScript script = file(SOURCE_05c);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_05d = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  def fct {",
				"    synchronized (this) {this.field = 2}",
				"  }",
				"  def fct2 : int {",
				"    synchronized (this) {return this.field}",
				"  }",
				"}");

		@Test
		public void testParser05d() throws Exception {
			SarlScript script = file(SOURCE_05d);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_06a = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private synchronized def fct {",
				"    this.field = 2",
				"  }",
				"  private synchronized def fct2 : int {",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser06a() throws Exception {
			SarlScript script = file(SOURCE_06a);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_06b = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private synchronized def fct {",
				"    this.field = 2",
				"  }",
				"  private def fct2 : int {",
				"    synchronized (this) {return this.field}",
				"  }",
				"}");

		@Test
		public void testParser06b() throws Exception {
			SarlScript script = file(SOURCE_06b);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_06c = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private def fct {",
				"    synchronized (this) {this.field = 2}",
				"  }",
				"  private synchronized def fct2 : int {",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser06c() throws Exception {
			SarlScript script = file(SOURCE_06c);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_06d = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private def fct {",
				"    synchronized (this) {this.field = 2}",
				"  }",
				"  private def fct2 : int {",
				"    synchronized (this) {return this.field}",
				"  }",
				"}");

		@Test
		public void testParser06d() throws Exception {
			SarlScript script = file(SOURCE_06d);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_07a = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private synchronized def fct : int {",
				"    this.field = 2",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser07a() throws Exception {
			SarlScript script = file(SOURCE_07a);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_07b = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private def fct : int {",
				"    synchronized(this) {this.field = 2",
				"    return this.field}",
				"  }",
				"}");

		@Test
		public void testParser07b() throws Exception {
			SarlScript script = file(SOURCE_07b);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_07c = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private def fct : int {",
				"    synchronized(this) {this.field = 2}",
				"    synchronized(this) {return this.field}",
				"  }",
				"}");

		@Test
		public void testParser07c() throws Exception {
			SarlScript script = file(SOURCE_07c);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

		private static final String SOURCE_07d = multilineString(
				"package io.sarl.lang.tests.bug208",
				"agent C1 {",
				"  var field = 1",
				"  private synchronized def fct : int {",
				"    synchronized(this) {this.field = 2}",
				"    return this.field",
				"  }",
				"}");

		@Test
		public void testParser07d() throws Exception {
			SarlScript script = file(SOURCE_07d);
			validate(script).assertNoWarnings(
					XbasePackage.eINSTANCE.getXExpression(),
					IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
		}

	}

}
