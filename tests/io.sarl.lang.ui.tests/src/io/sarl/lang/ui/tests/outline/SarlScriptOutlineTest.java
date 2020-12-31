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

import org.junit.Test;

/** Test the outline of a sarl script.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlScriptOutlineTest extends AbstractSARLOutlineTreeProviderTest {

	/**
	 * @throws Exception
	 */
	@Test
	public void full() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"package io.sarl.lang.ui.tests.outline.tests\n" //$NON-NLS-1$
				+ "import io.sarl.lang.core.Event\n" //$NON-NLS-1$
				+ "event MyEvent\n" //$NON-NLS-1$
				+ "capacity C1 {\n" //$NON-NLS-1$
				+ "    def myfct\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "capacity C2 {\n" //$NON-NLS-1$
				+ "    def myfct2\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "skill MySkill implements C1 {\n" //$NON-NLS-1$
				+ "    def myfct { }\n" //$NON-NLS-1$
				+ "    var z : float\n" //$NON-NLS-1$
				+ "    var y : float\n" //$NON-NLS-1$
				+ "    var x : float\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "agent ABC {\n" //$NON-NLS-1$
				+ "    on Event {\n" //$NON-NLS-1$
				+ "        println(\"Receiving the event \"+occurrence)\n" //$NON-NLS-1$
				+ "    }\n" //$NON-NLS-1$
				+ "    uses C1\n" //$NON-NLS-1$
				+ "    on MyEvent {\n" //$NON-NLS-1$
				+ "        setSkill(typeof(C1), new MySkill)\n" //$NON-NLS-1$
				+ "    }\n" //$NON-NLS-1$
				+ "    uses C2\n" //$NON-NLS-1$
				+ "}\n" //$NON-NLS-1$
				+ "// END\n"); //$NON-NLS-1$
		asserts.numChildren(7);
		OutlineAsserts c, cc;
		asserts.leaf(0, "io.sarl.lang.ui.tests.outline.tests"); //$NON-NLS-1$

		c = asserts.nextChild("import declarations"); //$NON-NLS-1$
		c.numChildren(1);
		c.leaf(0, "io.sarl.lang.core.Event").leaf(); //$NON-NLS-1$

		c = asserts.nextChild("ABC"); //$NON-NLS-1$
		c.numChildren(3);
		cc = c.child(0, "capacity uses"); //$NON-NLS-1$
		cc.numChildren(2);
		cc.leaf(0, "C1"); //$NON-NLS-1$
		cc.leaf(1, "C2"); //$NON-NLS-1$
		c.nextChild("on Event").leaf(); //$NON-NLS-1$
		c.nextChild("on MyEvent").leaf(); //$NON-NLS-1$

		c = asserts.nextChild("C1"); //$NON-NLS-1$
		c.numChildren(1);
		c.leaf(0, "myfct() : void"); //$NON-NLS-1$

		c = asserts.nextChild("C2"); //$NON-NLS-1$
		c.numChildren(1);
		c.leaf(0, "myfct2() : void"); //$NON-NLS-1$

		asserts.nextChild("MyEvent").leaf(); //$NON-NLS-1$

		c = asserts.nextChild("MySkill"); //$NON-NLS-1$
		c.numChildren(4);
		c.leaf(0, "x : float"); //$NON-NLS-1$
		c.leaf(1, "y : float"); //$NON-NLS-1$
		c.leaf(2, "z : float"); //$NON-NLS-1$
		c.leaf(3, "myfct() : void"); //$NON-NLS-1$
	}

}
