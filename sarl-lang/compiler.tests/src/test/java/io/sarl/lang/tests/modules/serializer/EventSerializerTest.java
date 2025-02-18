/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
package io.sarl.lang.tests.modules.serializer;

import static io.sarl.lang.tests.api.tools.TestEObjects.event;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("serialization: event")
@Tag("core")
@Tag("serialization")
@SuppressWarnings("all")
public class EventSerializerTest {

	@DisplayName("Without type parameter")
	@Nested
	public class WithoutTypeParameter extends AbstractSerializerTest {
		
		@Test
		@DisplayName("event")
		public void empty_noBlock_noSuper() throws Exception {
			String s = "event Foo"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event {}")
		public void empty_block_noSuper() throws Exception {
			String s = "event Foo { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event extends")
		public void empty_noBlock_super() throws Exception {
			String s = "event Foo extends foo.ecore.SubEvent"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event extends {}")
		public void empty_block_super() throws Exception {
			String s = "event Foo extends foo.ecore.SubEvent { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

	}

	@DisplayName("With type parameters")
	@Nested
	public class WithTypeParameters extends AbstractSerializerTest {

		@Test
		@DisplayName("event<A>")
		public void empty_noBlock_noSuper_0() throws Exception {
			String s = "event Foo<A>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number>")
		public void empty_noBlock_noSuper_1() throws Exception {
			String s = "event Foo<A extends Number>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B>")
		public void empty_noBlock_noSuper_2() throws Exception {
			String s = "event Foo<A extends Number, B>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B extends String>")
		public void empty_noBlock_noSuper_3() throws Exception {
			String s = "event Foo<A extends Number, B extends String>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B extends String>")
		public void empty_noBlock_noSuper_4() throws Exception {
			String s = "event Foo<A, B extends String>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B>")
		public void empty_noBlock_noSuper_5() throws Exception {
			String s = "event Foo<A, B>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A> {}")
		public void empty_block_noSuper_0() throws Exception {
			String s = "event Foo<A> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number> {}")
		public void empty_block_noSuper_1() throws Exception {
			String s = "event Foo<A extends Number> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B> {}")
		public void empty_block_noSuper_2() throws Exception {
			String s = "event Foo<A extends Number, B> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B extends String> {}")
		public void empty_block_noSuper_3() throws Exception {
			String s = "event Foo<A extends Number, B extends String> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B extends String> {}")
		public void empty_block_noSuper_4() throws Exception {
			String s = "event Foo<A, B extends String> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B> {}")
		public void empty_block_noSuper_5() throws Exception {
			String s = "event Foo<A, B> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A> extends")
		public void empty_noBlock_super_0() throws Exception {
			String s = "event Foo<A> extends foo.ecore.SubEvent"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number> extends")
		public void empty_noBlock_super_1() throws Exception {
			String s = "event Foo<A extends Number> extends foo.ecore.SubEvent"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B> extends")
		public void empty_noBlock_super_2() throws Exception {
			String s = "event Foo<A extends Number, B> extends foo.ecore.SubEvent"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B extends String> extends")
		public void empty_noBlock_super_3() throws Exception {
			String s = "event Foo<A extends Number, B extends String> extends foo.ecore.SubEvent"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B extends String> extends")
		public void empty_noBlock_super_4() throws Exception {
			String s = "event Foo<A, B extends String> extends foo.ecore.SubEvent"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B> extends")
		public void empty_noBlock_super_5() throws Exception {
			String s = "event Foo<A, B> extends foo.ecore.SubEvent"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A> extends {}")
		public void empty_block_super_0() throws Exception {
			String s = "event Foo<A> extends foo.ecore.SubEvent { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number> extends {}")
		public void empty_block_super_1() throws Exception {
			String s = "event Foo<A extends Number> extends foo.ecore.SubEvent { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B> extends {}")
		public void empty_block_super_2() throws Exception {
			String s = "event Foo<A extends Number, B> extends foo.ecore.SubEvent { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B extends String> extends {}")
		public void empty_block_super_3() throws Exception {
			String s = "event Foo<A extends Number, B extends String> extends foo.ecore.SubEvent { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B extends String> extends {}")
		public void empty_block_super_4() throws Exception {
			String s = "event Foo<A, B extends String> extends foo.ecore.SubEvent { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B> extends {}")
		public void empty_block_super_5() throws Exception {
			String s = "event Foo<A,B> extends foo.ecore.SubEvent { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A> extends <Double>")
		public void empty_noBlock_supergen_0() throws Exception {
			String s = "event Foo<A> extends foo.ecore.SubGenEvent<Double>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number> extends <A>")
		public void empty_noBlock_supergen_1() throws Exception {
			String s = "event Foo<A extends Number> extends foo.ecore.SubGenEvent<A>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B> extends <A>")
		public void empty_noBlock_supergen_2() throws Exception {
			String s = "event Foo<A extends Number, B> extends foo.ecore.SubGenEvent<A>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B extends String> extends <A>")
		public void empty_noBlock_supergen_3() throws Exception {
			String s = "event Foo<A extends Number, B extends String> extends foo.ecore.SubGenEvent<A>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B extends String> extends <Double>")
		public void empty_noBlock_supergen_4() throws Exception {
			String s = "event Foo<A, B extends String> extends foo.ecore.SubGenEvent<Double>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B> extends <Double>")
		public void empty_noBlock_supergen_5() throws Exception {
			String s = "event Foo<A, B> extends foo.ecore.SubGenEvent<Double>"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A> extends <Double> {}")
		public void empty_block_supergen_0() throws Exception {
			String s = "event Foo<A> extends foo.ecore.SubGenEvent<Double> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number> extends <A> {}")
		public void empty_block_superen_1() throws Exception {
			String s = "event Foo<A extends Number> extends foo.ecore.SubGenEvent<A> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B> extends <A> {}")
		public void empty_block_supergen_2() throws Exception {
			String s = "event Foo<A extends Number, B> extends foo.ecore.SubGenEvent<A> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A extends Number, B extends String> extends <A> {}")
		public void empty_block_supergen_3() throws Exception {
			String s = "event Foo<A extends Number, B extends String> extends foo.ecore.SubGenEvent<A> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B extends String> extends <Double> {}")
		public void empty_block_supergen_4() throws Exception {
			String s = "event Foo<A, B extends String> extends foo.ecore.SubGenEvent<Double> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

		@Test
		@DisplayName("event<A, B> extends <Double> {}")
		public void empty_block_supergen_5() throws Exception {
			String s = "event Foo<A,B> extends foo.ecore.SubGenEvent<Double> { }"; //$NON-NLS-1$
			this.object = event(getParseHelper(), getValidationHelper(), s);
			assertSerialize(s);
		}

	}

}
