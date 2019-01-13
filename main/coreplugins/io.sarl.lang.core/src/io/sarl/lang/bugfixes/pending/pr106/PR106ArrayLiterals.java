/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.bugfixes.pending.pr106;

import com.google.common.annotations.GwtCompatible;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Extend the ArrayLiterals class for creating multi-dimensional arrays.
 *
 * <p>FIXME: Remove when PR is merged: https://github.com/eclipse/xtext-lib/pull/106
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-lib/pull/106"
 */
@SuppressWarnings("checkstyle:all")
@GwtCompatible
public class PR106ArrayLiterals {

	/**
	 * @param size0
	 *            the first size for the array to be created
	 * @param size1
	 *            the second size for the array to be created
	 * @return an array of the given sizes
	 * @since 15.0
	 */
	@Pure
	@Inline("new $4[$1][$2]")
	public static <T> T[][] newArrayOfSize(int size0, int size1) {
		throw new UnsupportedOperationException(
				"This method relies on the inlined compilation (see @Inline annotation), and cannot be used from Java or with an uncustomized interpreter."); //$NON-NLS-1$
	}

	/**
	 * @param size0
	 *            the first size for the array to be created
	 * @param size1
	 *            the second size for the array to be created
	 * @return an array of the given sizes
	 * @since 15.0
	 */
	@Pure
	@Inline("new char[$1][$2]")
	public static char[][] newCharArrayOfSize(int size0, int size1) {
		return new char[size0][size1];
	}

	/**
	 * @param size0
	 *            the first size for the array to be created
	 * @param size1
	 *            the second size for the array to be created
	 * @return an array of the given sizes
	 * @since 15.0
	 */
	@Pure
	@Inline("new int[$1][$2]")
	public static int[][] newIntArrayOfSize(int size0, int size1) {
		return new int[size0][size1];
	}

	/**
	 * @param size0
	 *            the first size for the array to be created
	 * @param size1
	 *            the second size for the array to be created
	 * @return an array of the given sizes
	 * @since 15.0
	 */
	@Pure
	@Inline("new boolean[$1][$2]")
	public static boolean[][] newBooleanArrayOfSize(int size0, int size1) {
		return new boolean[size0][size1];
	}

	/**
	 * @param size0
	 *            the first size for the array to be created
	 * @param size1
	 *            the second size for the array to be created
	 * @return an array of the given sizes
	 * @since 15.0
	 */
	@Pure
	@Inline("new short[$1][$2]")
	public static short[][] newShortArrayOfSize(int size0, int size1) {
		return new short[size0][size1];
	}

	/**
	 * @param size0
	 *            the first size for the array to be created
	 * @param size1
	 *            the second size for the array to be created
	 * @return an array of the given sizes
	 * @since 15.0
	 */
	@Pure
	@Inline("new long[$1][$2]")
	public static long[][] newLongArrayOfSize(int size0, int size1) {
		return new long[size0][size1];
	}

	/**
	 * @param size0
	 *            the first size for the array to be created
	 * @param size1
	 *            the second size for the array to be created
	 * @return an array of the given sizes
	 * @since 15.0
	 */
	@Pure
	@Inline("new float[$1][$2]")
	public static float[][] newFloatArrayOfSize(int size0, int size1) {
		return new float[size0][size1];
	}

	/**
	 * @param size0
	 *            the first size for the array to be created
	 * @param size1
	 *            the second size for the array to be created
	 * @return an array of the given sizes
	 * @since 15.0
	 */
	@Pure
	@Inline("new double[$1][$2]")
	public static double[][] newDoubleArrayOfSize(int size0, int size1) {
		return new double[size0][size1];
	}

	/**
	 * @param size0
	 *            the first size for the array to be created
	 * @param size1
	 *            the second size for the array to be created
	 * @return an array of the given sizes
	 * @since 15.0
	 */
	@Pure
	@Inline("new byte[$1][$2]")
	public static byte[][] newByteArrayOfSize(int size0, int size1) {
		return new byte[size0][size1];
	}

}
