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

package io.sarl.eclipse.natures;

import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.inject.Inject;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * Action for removing the SARL nature to a project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class RemoveSarlNatureHandler extends AbstractHandler {

	@Inject
	private IProjectUnconfigurator configurator;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		final ISelection currentSelection = HandlerUtil.getCurrentSelection(event);
		if (currentSelection instanceof IStructuredSelection) {
			final IStructuredSelection structuredSelection = (IStructuredSelection) currentSelection;
			final List<IProject> projects = new ArrayList<>();
			final Iterator<?> iterator = structuredSelection.iterator();
			while (iterator.hasNext()) {
				final Object obj = iterator.next();
				if (obj instanceof IJavaProject) {
					projects.add(((IJavaProject) obj).getProject());
				} else if (obj instanceof IProject) {
					projects.add((IProject) obj);
				}
			}
			final Shell activeShell = HandlerUtil.getActiveShell(event);
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
		monitor.setTaskName(MessageFormat.format(Messages.RemoveSarlNatureHandler_2, project.getName()));
		final SubMonitor mon = SubMonitor.convert(monitor, 2);
		if (this.configurator.canUnconfigure(project, mon.newChild(1))) {
			try {
				this.configurator.unconfigure(project, mon.newChild(1));
			} catch (CoreException exception) {
				throw new ExecutionException(exception.getLocalizedMessage(), exception);
			}
		}
		monitor.done();
	}

}
