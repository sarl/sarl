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
package io.sarl.lang.ui.tests.outline;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.google.common.base.Joiner;
import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.XtextDocument;
import org.eclipse.xtext.ui.editor.outline.IOutlineNode;
import org.eclipse.xtext.ui.editor.outline.impl.OutlineFilterAndSorter;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.outline.SARLOutlineTreeProvider;
import io.sarl.tests.api.AbstractSarlUiTest;

/** Abstract implementation for the outline tests.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSARLOutlineTreeProviderTest extends AbstractSarlUiTest {

	@Inject
	private SARLOutlineTreeProvider treeProvider;

	@Inject
	private OutlineFilterAndSorter sorter;

	@Inject
	private MockedSARLOutlineNodeComparator comparator;

	@Inject
	private Provider<XtextDocument> documentProvider;

	/** This rule permits to initialize the outline tests.
	 */
	@Rule
	public TestWatcher sarlOutlineWatchter = new TestWatcher() {
		@SuppressWarnings("synthetic-access")
		@Override
		protected void starting(Description description) {
			AbstractSARLOutlineTreeProviderTest.this.sorter.setComparator(AbstractSARLOutlineTreeProviderTest.this.comparator);
		}
	};

	/** Replies the outline sorted.
	 *
	 * @return the outline sorter.
	 */
	protected OutlineFilterAndSorter getSorter() {
		return this.sorter;
	}

	/** Create a new assertion tool on a specific code resource.
	 * The code will be used for generating the outline.
	 *
	 * @param code the code.
	 * @return the assertion tool.
	 * @throws Exception
	 * @throws CoreException
	 */
	protected OutlineAsserts newOutlineAsserts(String code) throws Exception, CoreException {
		SarlScript script = helper().sarlScript(helper().generateFilename(), code);
		return newOutlineAsserts(script);
	}

	/** Create a new assertion tool on a specific code resource.
	 *
	 * @param script the script to consider for producing the outline.
	 * @return the assertion tool.
	 * @throws Exception
	 * @throws CoreException
	 */
	protected OutlineAsserts newOutlineAsserts(SarlScript script) throws Exception, CoreException {
		XtextDocument document = this.documentProvider.get();
		document.setInput((XtextResource) script.eResource());
		IOutlineNode root = this.treeProvider.createRoot(document);
		OutlineAsserts assertBuilder = new OutlineAsserts(root);
		return assertBuilder;
	}

	/** Provides tools for asserting on the outline structure.
	 * Copied from Xtext project.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected class OutlineAsserts {

		private final IOutlineNode node;
		private int index = -1;

		/**
		 * @param node
		 */
		public OutlineAsserts(IOutlineNode node) {
			this.node = node;
		}

		/**
		 * @param node
		 * @param text
		 */
		public OutlineAsserts(IOutlineNode node, String text) {
			this(node);
			assertEquals(text, node.getText().toString());
		}

		/**
		 * @param node
		 * @param styledText
		 */
		public OutlineAsserts(IOutlineNode node, StyledString styledText) {
			this(node);
			Object text = node.getText();
			assertTrue("Node text is not a StyledString: " //$NON-NLS-1$
					+ node, text instanceof StyledString);
			StyledString styledString = (StyledString) text;
			assertEquals("StyledString-text doesn't match", //$NON-NLS-1$
					styledText.getString(), styledString.getString());
			assertArrayEquals("StyledString-StyleRanges don't match", //$NON-NLS-1$
					styledText.getStyleRanges(),
					styledString.getStyleRanges());
		}

		/** Assert the number of children for the node is equal
		 * to the given value.
		 *
		 * @param num expected number of children.
		 * @return this
		 */
		public OutlineAsserts numChildren(int num) {
			IOutlineNode[] filteredAndSortedChildren = getSorter().filterAndSort(
					this.node.getChildren());
			assertEquals("Wrong number of children\n" //$NON-NLS-1$
					+ Joiner.on("\n").join(filteredAndSortedChildren), num, //$NON-NLS-1$
					filteredAndSortedChildren.length);
			return this;
		}

		/** Assert the number of children for the node is strictly greater
		 * than the given value.
		 *
		 * @param num expected number of children.
		 * @return this
		 */
		public OutlineAsserts numChildrenGreaterThan(int num) {
			IOutlineNode[] filteredAndSortedChildren = getSorter().filterAndSort(
					this.node.getChildren());
			assertTrue("Wrong number of children\n" //$NON-NLS-1$
					+ Joiner.on("\n").join(filteredAndSortedChildren), //$NON-NLS-1$
					filteredAndSortedChildren.length > num);
			return this;
		}

		/** Assert the number of children for the node is strictly lower
		 * than the given value.
		 *
		 * @param num expected number of children.
		 * @return this
		 */
		public OutlineAsserts numChildrenLowerThan(int num) {
			IOutlineNode[] filteredAndSortedChildren = getSorter().filterAndSort(
					this.node.getChildren());
			assertTrue("Wrong number of children\n" //$NON-NLS-1$
					+ Joiner.on("\n").join(filteredAndSortedChildren), //$NON-NLS-1$
					filteredAndSortedChildren.length < num);
			return this;
		}

		/** Assert that a text region is inside the node.
		 *
		 * @param hasTextRegion expected value
		 * @return this
		 */
		public OutlineAsserts hasTextRegion(boolean hasTextRegion) {
			if (hasTextRegion) {
				assertNotNull(this.node.getSignificantTextRegion());
			} else {
				assertNull(this.node.getSignificantTextRegion());
			}
			return this;
		}

		/** Replies an assert builder on a child.
		 *
		 * @param index position of the child.
		 * @param text the text associated to the child in the outline.
		 * @return the asserts for the child.
		 */
		public OutlineAsserts child(int index, StyledString text) {
			IOutlineNode[] sortedChildren = getSorter().filterAndSort(this.node.getChildren());
			if (sortedChildren.length <= index)
				fail("Missing child node " + index); //$NON-NLS-1$
			return new OutlineAsserts(sortedChildren[index], text);
		}

		/** Replies an assert builder on a child.
		 *
		 * @param index position of the child.
		 * @param text the text associated to the child in the outline.
		 * @return the asserts for the child.
		 */
		public OutlineAsserts child(int index, String text) {
			IOutlineNode[] sortedChildren = getSorter().filterAndSort(this.node.getChildren());
			if (sortedChildren.length <= index)
				fail("Missing child node " + index); //$NON-NLS-1$
			this.index = index;
			return new OutlineAsserts(sortedChildren[index], text);
		}

		/** Move to the next child.
		 *
		 * @param text the text associated to the child in the outline
		 * @return the asserts for the child.
		 */
		public OutlineAsserts nextChild(String text) {
			this.index = this.index + 1;
			return child(this.index, text);
		}

		/** Assert that the child is a leaf.
		 *
		 * @param index position of the child.
		 * @param text the text associated to the child.
		 * @return the asserts for the child.
		 */
		public OutlineAsserts leaf(int index, String text) {
			return child(index, text).leaf();
		}

		/** Assert that it is a leaf.
		 *
		 * @return this.
		 */
		public OutlineAsserts leaf() {
			return numChildren(0);
		}

		/** Assert that the child is a leaf.
		 *
		 * @param index position of the child.
		 * @param text the text associated to the child.
		 * @return the asserts for the child.
		 */
		public OutlineAsserts notLeaf(int index, String text) {
			return child(index, text).numChildrenGreaterThan(0);
		}

	} // class OutlineAsserts

}
