/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.ui.bugs.bug1115;

import java.util.Iterator;
import java.util.Map;
import java.util.Queue;

import com.google.common.collect.Maps;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.emf.common.util.URI;
import org.eclipse.xtext.common.types.ui.editor.JvmTypesAwareDirtyStateEditorSupport;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.impl.ResourceDescriptionChangeEvent;
import org.eclipse.xtext.util.Pair;
import org.eclipse.xtext.util.Tuples;

import io.sarl.lang.ui.internal.LangActivator;
import io.sarl.lang.util.ReflectField;

/** Support for dirty resources in the editor.
 *
 * <p>This class provides a fix for Issue #1115: Eclipse error on the editor state.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1115"
 */
public class FixedDirtyStateEditorSupport extends JvmTypesAwareDirtyStateEditorSupport {

	private static final ISchedulingRule SCHEDULING_RULE = ReflectField.of(FixedDirtyStateEditorSupport.class, ISchedulingRule.class, "SCHEDULING_RULE").get(); //$NON-NLS-1$

	@Override
	protected UpdateEditorStateJob createUpdateEditorJob() {
		// default is sequential execution to ensure a minimum number of
		// spawned worker threads
		return new FixedUpdateEditorStateJob(SCHEDULING_RULE);
	}

	/** Job for updated the editor state.
	 *
	 * <p>This class provides a fix for Issue #1115: Eclipse error on the editor state.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @see "https://github.com/sarl/sarl/issues/1115"
	 */
	protected class FixedUpdateEditorStateJob extends UpdateEditorStateJob {

		@SuppressWarnings("rawtypes")
		private final ReflectField<FixedUpdateEditorStateJob, Queue> hpendingChanges = ReflectField.of(
				FixedUpdateEditorStateJob.class, Queue.class, "pendingChanges"); //$NON-NLS-1$
		
		/** Constructor.
		 *
		 * @param rule the scheduling rule to be used by this job.
		 */
		protected FixedUpdateEditorStateJob(ISchedulingRule rule) {
			super(rule);
		}

		/** Re-implementation to avoid any error regarding the fact that the URI of the delta cannot be retreived.
		 */
		@SuppressWarnings("synthetic-access")
		@Override
		protected Pair<IResourceDescription.Event, Integer> mergePendingDeltas() {
			Map<URI, IResourceDescription.Delta> uriToDelta = Maps.newLinkedHashMap();
			@SuppressWarnings("unchecked")
			Iterator<IResourceDescription.Delta> iter = this.hpendingChanges.get(this).iterator();
			int size = 0;
			while(iter.hasNext()) {
				IResourceDescription.Delta delta = iter.next();
				URI uri = null;
				try {
					uri = delta.getUri();
				} catch (IllegalStateException ex) {
					uri = null;
				}
				if (uri != null) {
					IResourceDescription.Delta prev = uriToDelta.get(uri);
					if (prev == null) {
						uriToDelta.put(uri, delta);
					} else if (prev.getOld() != delta.getNew()){
						uriToDelta.put(uri, createDelta(delta, prev));
					} else {
						uriToDelta.remove(uri);
					}
					size++;
				} else {
					LangActivator.getInstance().getLog().log(
							new Status(IStatus.ERROR, LangActivator.PLUGIN_ID,
							Messages.FixedDirtyStateEditorSupport_0));
				}
			}
			IResourceDescription.Event event = new ResourceDescriptionChangeEvent(uriToDelta.values());
			return Tuples.create(event, size);
		}
	}

}
