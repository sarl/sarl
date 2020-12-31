/*
 * $Id$
 *
 * SARL is an general-purpose capacity programming language.
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
package io.sarl.lang.ui.tests.outline;

import org.junit.Test;

/** Test the outline of the "capacity" statement.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class CapacityOutlineTest extends AbstractSARLOutlineTreeProviderTest {

	/**
	 * @throws Exception
	 */
	@Test
	public void empty() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"capacity C1 {}"); //$NON-NLS-1$
		asserts.numChildren(1);
		asserts.leaf(0, "C1"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Action_0() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"capacity C1 { def operation : boolean }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "C1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "operation() : boolean"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Action_1() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"capacity C1 { def operation(a : float) }"); //$NON-NLS-1$
		asserts.child(0, "C1") //$NON-NLS-1$
			.leaf(0, "operation(float) : void"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Action_2() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"capacity C1 { def operation(a : float) : int }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "C1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "operation(float) : int"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test2Actions_0() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"capacity C1 { def xyz : boolean def abc : float }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "C1"); //$NON-NLS-1$
		a.numChildren(2);
		a.leaf(0, "abc() : float"); //$NON-NLS-1$
		a.leaf(1, "xyz() : boolean"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test2Actions_1() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"capacity C1 { def xyz(b : char) : boolean def abc : float }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "C1"); //$NON-NLS-1$
		a.numChildren(2);
		a.leaf(0, "abc() : float"); //$NON-NLS-1$
		a.leaf(1, "xyz(char) : boolean"); //$NON-NLS-1$
	}

}
