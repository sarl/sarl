/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.scoping.numbers;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.eclipse.xtext.xbase.lib.Inline;

/** Provide static operators for atomic numbers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
public final class AtomicNumbersExtensions {

	private AtomicNumbersExtensions() {
		//
	}

	/**
	 *The postfix <code>decrement</code> operator. This is the equivalent to the Java's <code>--</code> postfix function.
	 *
	 * @param i a number.
	 * @return   <code>i--</code>
	 *@since 2.6
	 */
	@Inline(value = "($1).getAndDecrement()")
	public static int operator_minusMinus(AtomicInteger i) {
		return i.getAndDecrement();
	}

	/**
	 *The postfix <code>decrement</code> operator. This is the equivalent to the Java's <code>--</code> postfix function.
	 *
	 * @param i a number.
	 * @return   <code>i--</code>
	 *@since 2.6
	 */
	@Inline(value = "($1).getAndDecrement()")
	public static long operator_minusMinus(AtomicLong i) {
		return i.getAndDecrement();
	}

	/**
	 *The postfix <code>increment</code> operator. This is the equivalent to the Java's <code>++</code> postfix function.
	 *
	 * @param i a number.
	 * @return   <code>i++</code>
	 *@since 2.6
	 */
	@Inline(value = "($1).getAndIncrement()")
	public static int operator_plusPlus(AtomicInteger i) {
		return i.getAndIncrement();
	}

	/**
	 *The postfix <code>increment</code> operator. This is the equivalent to the Java's <code>++</code> postfix function.
	 *
	 * @param i a number.
	 * @return   <code>i++</code>
	 *@since 2.6
	 */
	@Inline(value = "($1).getAndIncrement()")
	public static long operator_plusPlus(AtomicLong i) {
		return i.getAndIncrement();
	}

}

