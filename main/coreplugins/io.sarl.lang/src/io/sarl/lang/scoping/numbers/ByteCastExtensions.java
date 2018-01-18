/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

// THIS FILE IS AUTO-GENERATED. DO NOT CHANGE MANUALLY

package io.sarl.lang.scoping.numbers;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provide static functions related to the casting of numbers of type {@code Byte}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
public final class ByteCastExtensions {

	private ByteCastExtensions() {
		//
	}

	// BEGIN GENERATED BLOCK

	/** Convert the given value to {@code AtomicLong}.
	 *
	 * @param a a number of {@code Byte} type.
	 * @return the equivalent value to {@code a} of {@code AtomicLong} type.
	 */
	@Pure
	@Inline(value = "$3.pow(($1)a.byteValue(), ($2)b.byteValue())", imported = Math.class)
	public static AtomicLong toAtomicLong(Byte a) {
		return new AtomicLong(a.byteValue());
	}

	/** Convert the given value to {@code AtomicInteger}.
	 *
	 * @param a a number of {@code Byte} type.
	 * @return the equivalent value to {@code a} of {@code AtomicInteger} type.
	 */
	@Pure
	@Inline(value = "$3.pow(($1)a.byteValue(), ($2)b.byteValue())", imported = Math.class)
	public static AtomicInteger toAtomicInteger(Byte a) {
		return new AtomicInteger(a.byteValue());
	}

	// END GENERATED BLOCK

}
