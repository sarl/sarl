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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.ui.tasks;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.xtext.preferences.IPreferenceValuesProvider;
import org.eclipse.xtext.preferences.PreferenceKey;
import org.eclipse.xtext.tasks.ITaskTagProvider;
import org.eclipse.xtext.tasks.PreferenceTaskTagProvider;
import org.eclipse.xtext.tasks.TaskTags;

/** Provider for task tags.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("restriction")
public class SarlTaskTagProvider implements ITaskTagProvider {

	@Inject
	private IPreferenceValuesProvider preferenceValuesProvider;

	@Override
	public TaskTags getTaskTags(Resource resource) {
		final var prefs = this.preferenceValuesProvider.getPreferenceValues(resource);
		final var namePref = prefs.getPreference(new PreferenceKey(
				JavaCore.COMPILER_TASK_TAGS, "TODO,FIXME,XXX")); //$NON-NLS-1$
		final var prioritiesPref = prefs.getPreference(new PreferenceKey(
				JavaCore.COMPILER_TASK_PRIORITIES, "NORMAL,HIGH,NORMAL")); //$NON-NLS-1$
		final var caseSensitivePref = prefs.getPreference(new PreferenceKey(
				JavaCore.COMPILER_TASK_CASE_SENSITIVE, JavaCore.ENABLED));
		final var tags = PreferenceTaskTagProvider.parseTags(namePref, prioritiesPref);

		final var taskTags = new TaskTags();
		taskTags.setCaseSensitive(caseSensitivePref.equals(JavaCore.ENABLED));
		taskTags.getTaskTags().addAll(tags);
		return taskTags;
	}

}
