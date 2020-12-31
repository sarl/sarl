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
package io.sarl.lang.ui.tests.highlighting;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collection;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import org.eclipse.xtext.ide.editor.syntaxcoloring.IHighlightedPositionAcceptor;
import org.eclipse.xtext.ide.editor.syntaxcoloring.ISemanticHighlightingCalculator;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultHighlightingConfiguration;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.TextRegion;
import org.eclipse.xtext.xbase.ui.highlighting.XbaseHighlightingConfiguration;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlUiTest;

/**
 * Unit tests for the higlighting feature dedicated to SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSARLHighlightingCalculatorTest extends AbstractSarlUiTest {

	/** Replies the instance of the calculator to test.
	 *
	 * @return the highlight calculator.
	 */
	protected abstract ISemanticHighlightingCalculator getCalculator();

	/** Highlight the given text.
	 *
	 * @param code the SARL script.
	 * @return the assertion checker.
	 */
	@SuppressWarnings("synthetic-access")
	protected HighlightingAsserts highlight(String code) {
		try {
			HighlightingAsserts asserts = new HighlightingAsserts();
			SarlScript script = file(code);
			getCalculator().provideHighlightingFor(
					(XtextResource) script.eResource(),
					asserts,
					CancelIndicator.NullImpl);
			return asserts;
		} catch(Exception e) {
			throw new RuntimeException(e);
		}
	}

	/** Provides tools for highlighting assertions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static final class HighlightingAsserts implements IHighlightedPositionAcceptor {

		private final Multimap<TextRegion, String> regions = HashMultimap.create();

		private int index = 0;

		private HighlightingAsserts() {
			//
		}

		/** {@inheritDoc}
		 * Copied from Xtext.
		 */
		@Override
		public final void addPosition(int offset, int length, String... id) {
			assertTrue("length = " + length, length >= 0); //$NON-NLS-1$
			TextRegion region = new TextRegion(offset, length);
			assertEquals(1, id.length);
			this.regions.put(region, id[0]);
		}

		/** Assert that the given region is marked with the given highlight id.
		 *
		 * @param offset
		 * @param length
		 * @param highlightID
		 * @return this
		 */
		public HighlightingAsserts is(int offset, int length, String highlightID) {
			TextRegion region = new TextRegion(offset, length);
			assertFalse(region.toString() + " is not contained in " //$NON-NLS-1$
					+ this.regions, this.regions.isEmpty());
			Collection<String> ids = this.regions.get(region);
			if (ids == null) {
				fail("expected: " + region.toString() //$NON-NLS-1$
						+ " but was not found in: " //$NON-NLS-1$
						+ this.regions);
			} else if (ids.isEmpty() || !ids.contains(highlightID)) {
				fail("expected: " + region.toString() //$NON-NLS-1$
						+ " with '" + highlightID //$NON-NLS-1$
						+ "' but was not found in: " //$NON-NLS-1$
						+ this.regions);
			}
			this.index = offset + length;
			return this;
		}

		/** Assert that the given region is marked with the given highlight id.
		 * This function is using the offset embedded in this object.
		 *
		 * @param length
		 * @param highlightID
		 * @return this
		 */
		public HighlightingAsserts is(int length, String highlightID) {
			return is(this.index, length, highlightID);
		}

		/** Assert that the given region is not marked.
		 *
		 * @param offset
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts not(int offset, int length) {
			if (!this.regions.isEmpty()) {
				TextRegion region = new TextRegion(offset, length);
				if (this.regions.containsKey(region)) {
					fail("the region " + region.toString() //$NON-NLS-1$
							+ " was found in: " + this.regions); //$NON-NLS-1$
				}
			}
			this.index = offset + length;
			return this;
		}

		/** Assert that the given region is not marked.
		 * This function is using the offset embedded in this object.
		 *
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts not(int length) {
			return not(this.index, length);
		}

		/** Assert that the given region is not marked with the given highlight id.
		 *
		 * @param offset
		 * @param length
		 * @param highlightID
		 * @return this
		 */
		public HighlightingAsserts not(int offset, int length, String highlightID) {
			TextRegion region = new TextRegion(offset, length);
			assertFalse(region.toString() + " is not contained in " //$NON-NLS-1$
					+ this.regions, this.regions.isEmpty());
			Collection<String> ids = this.regions.get(region);
			if (ids == null) {
				fail("expected: " + region.toString() //$NON-NLS-1$
						+ " but was not found in: " //$NON-NLS-1$
						+ this.regions.keys());
			} else if (ids.isEmpty() || ids.contains(highlightID)) {
				fail("expected: " + region.toString() //$NON-NLS-1$
						+ " without '" + highlightID //$NON-NLS-1$
						+ "' but it was found."); //$NON-NLS-1$
			}
			this.index = offset + length;
			return this;
		}

		/** Assert that the given region is not marked with the given highlight id.
		 * This function is using the offset embedded in this object.
		 *
		 * @param length
		 * @param highlightID
		 * @return this
		 */
		public HighlightingAsserts not(int length, String highlightID) {
			return not(this.index, length, highlightID);
		}

		/** Assert that the given region is insignifiant.
		 * A region is insignifiant if it is not highlighted.
		 *
		 * @param offset
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts insignifiant(int offset, int length) {
			return is(offset, length, DefaultHighlightingConfiguration.DEFAULT_ID);
		}

		/** Assert that the given region is insignifiant.
		 * A region is insignifiant if it is not highlighted.
		 * This function is using the offset embedded in this object.
		 *
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts insignifiant(int length) {
			return insignifiant(this.index, length);
		}

		/** Assert that the given region is a keyword.
		 *
		 * @param offset
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts keyword(int offset, int length) {
			return is(offset, length, DefaultHighlightingConfiguration.KEYWORD_ID);
		}

		/** Assert that the given region is a keyword.
		 * This function is using the offset embedded in this object.
		 *
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts keyword(int length) {
			return keyword(this.index, length);
		}

		/** Assert that the given region is a punctuation.
		 *
		 * @param offset
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts punct(int offset, int length) {
			return is(offset, length, DefaultHighlightingConfiguration.PUNCTUATION_ID);
		}

		/** Assert that the given region is a punctuation.
		 * This function is using the offset embedded in this object.
		 *
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts punct(int length) {
			return punct(this.index, length);
		}

		/** Assert that the given region is a static method call.
		 *
		 * @param offset
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts staticMethodCall(int offset, int length) {
			return is(offset, length, XbaseHighlightingConfiguration.STATIC_METHOD_INVOCATION);
		}

		/** Assert that the given region is a static method call.
		 * This function is using the offset embedded in this object.
		 *
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts staticMethodCall(int length) {
			return staticMethodCall(this.index, length);
		}

		/** Assert that the given region is a static field call.
		 *
		 * @param offset
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts staticFieldCall(int offset, int length) {
			return is(offset, length, XbaseHighlightingConfiguration.STATIC_FIELD);
		}

		/** Assert that the given region is a static field call.
		 * This function is using the offset embedded in this object.
		 *
		 * @param length
		 * @return this
		 */
		public HighlightingAsserts staticFieldCall(int length) {
			return staticFieldCall(this.index, length);
		}

	}

}
