/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.ui.tests.tasks;

import static org.junit.Assert.*;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.tasks.SarlTaskTagProvider;
import io.sarl.tests.api.AbstractSarlUiTest;

import java.util.Iterator;

import org.eclipse.xtext.tasks.Priority;
import org.eclipse.xtext.tasks.TaskTag;
import org.eclipse.xtext.tasks.TaskTags;
import org.junit.Test;

import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SarlTaskTagProviderTest extends AbstractSarlUiTest {
	
	@Inject
	private SarlTaskTagProvider provider;
	
	@Test
	public void getTaskTags() throws Exception {
		SarlScript script = file(multilineString(
				"/** TODO This is a comment */",
				"agent Foo {",
				"/** FIXME You must fix this!",
				"*/",
				"def foo { // XXX: Inside the code",
				"}",
				"}",
				"//TODO: Complete the code!"),
				true);
		TaskTag tag;
		TaskTags tags = this.provider.getTaskTags(script.eResource());
		assertTrue(tags.isCaseSensitive());
		Iterator<TaskTag> iterator = tags.iterator();

		assertTrue(iterator.hasNext());
		tag = iterator.next();
		assertEquals("TODO", tag.getName());
		assertEquals(Priority.NORMAL, tag.getPriority());

		assertTrue(iterator.hasNext());
		tag = iterator.next();
		assertEquals("FIXME", tag.getName());
		assertEquals(Priority.HIGH, tag.getPriority());

		assertTrue(iterator.hasNext());
		tag = iterator.next();
		assertEquals("XXX", tag.getName());
		assertEquals(Priority.NORMAL, tag.getPriority());

		assertFalse(iterator.hasNext());
	}
	
}