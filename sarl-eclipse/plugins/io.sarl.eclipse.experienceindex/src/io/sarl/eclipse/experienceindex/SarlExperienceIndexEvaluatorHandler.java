/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.experienceindex;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.PlatformUI;

import io.sarl.api.experienceindex.SarlExperienceIndex;
import io.sarl.api.experienceindex.SarlExperienceIndex.SEI;

/**
 * Launch the computation of the SARL experience index in the Eclipse environment.
 *
 * <p>Launching the computation in the Eclipse environment may provide lower results
 * than on the command line.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlExperienceIndexEvaluatorHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) {
		final Task job = new Task();
		job.schedule();
		return null;
	}

	private static class Task extends Job {

		Task() {
			super(Messages.SarlExperienceIndexEvaluatorHandler_0);
		}

		@Override
		public IStatus run(IProgressMonitor monitor) {
			final SEI experienceIndex = SarlExperienceIndex.getJanusExperienceIndex(() -> monitor.isCanceled());
			if (!monitor.isCanceled()) {
				PlatformUI.getWorkbench().getDisplay().asyncExec(() -> {
					SarlExperienceIndexResultDialog.open(
							null,
							experienceIndex);
				});
			}
			return Status.OK_STATUS;
		}

	}

}
