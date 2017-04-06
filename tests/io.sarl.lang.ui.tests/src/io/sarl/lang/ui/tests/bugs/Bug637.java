/*
 * Copyright (C) 2014-2017 the original authors or authors.
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
package io.sarl.lang.ui.tests.bugs;

import org.junit.Test;

import io.sarl.lang.ui.tests.contentassist.AbstractContentAssistTest;

/** Issue: Product Panic when auto-completing.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/637"
 */
@SuppressWarnings("all")
public class Bug637 extends AbstractContentAssistTest {

	@Override
	protected String getPrefix() {
		return multilineString(
				"package fr.utbm.info.ia51.labwork2.environment",
				"",
				"import java.lang.ref.WeakReference",
				"import fr.utbm.info.ia51.framework.math.Rectangle2f",
				"",
				"/**",
				"* @author apette",
				"*",
				"*/",
				"class TreeNode {",
				"",
				"val parent : WeakReference<TreeNode>",
				"",
				"var children : TreeNode[]",
				"",
				"new (parent : TreeNode, box : Rectangle2f)",
				"{",
				"this.parent = new ");
	}

	@Override
	protected String getSuffix() {
		return multilineString(
				"",
				"}",
				"}"
				);
	}
	@Test
	public void completionAfterNewKeyword() throws Exception {
		assertTextInsideProposals(newBuilder(), "TreeNode");
	}

}
