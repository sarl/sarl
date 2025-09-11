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

package io.sarl.eclipse.natures;

import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.inject.Inject;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * Action for converting the nature of a project to SARL.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AddSarlNatureHandler extends AbstractHandler {

	@Inject
	private SARLProjectConfigurator configurator;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		final var currentSelection = HandlerUtil.getCurrentSelection(event);
		if (currentSelection instanceof IStructuredSelection structuredSelection) {
			final var projects = new ArrayList<IProject>();
			final var iterator = structuredSelection.iterator();
			while (iterator.hasNext()) {
				final var obj = iterator.next();
				if (obj instanceof IJavaProject cvalue) {
					projects.add(cvalue.getProject());
				} else if (obj instanceof IProject cvalue) {
					projects.add(cvalue);
				}
			}
			final var activeShell = HandlerUtil.getActiveShell(event);
			convertAllWithProgress(activeShell, projects);
		}
		return null;
	}

	private boolean convertAllWithProgress(Shell shell, List<IProject> projects) throws ExecutionException {
		final IRunnableWithProgress op = new IRunnableWithProgress() {
			@Override
			public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
				monitor.beginTask(Messages.RemoveSarlNatureHandler_0, projects.size());
				final SubMonitor mon = SubMonitor.convert(monitor);
				try {
					for (final IProject project : projects) {
						doConvert(project, mon.newChild(1));
					}
				} catch (ExecutionException e) {
					throw new InvocationTargetException(e);
				} finally {
					monitor.done();
				}
			}
		};
		try {
			new ProgressMonitorDialog(shell).run(true, true, op);
		} catch (InvocationTargetException exception) {
			throw new ExecutionException(
					MessageFormat.format(Messages.RemoveSarlNatureHandler_1, exception.getLocalizedMessage()), exception);
		} catch (InterruptedException exception) {
			//interrupted by user
			return true;
		}
		return false;
	}

	/** Convert the given project.
	 *
	 * @param project the project to convert..
	 * @param monitor the progress monitor.
	 * @throws ExecutionException if something going wrong.
	 */
	protected void doConvert(IProject project, IProgressMonitor monitor) throws ExecutionException {
		monitor.setTaskName(MessageFormat.format(Messages.AddSarlNatureHandler_2, project.getName()));
		final SubMonitor mon = SubMonitor.convert(monitor, 2);
		if (this.configurator.canConfigure(project, Collections.emptySet(), mon.newChild(1))) {
			this.configurator.configure(project, Collections.emptySet(), mon.newChild(1));
		}
		monitor.done();
	}

}
