/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.ui.editor;

import java.text.MessageFormat;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.google.common.base.Strings;
import com.google.inject.Inject;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.Resource.Diagnostic;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.PlatformUI;
import org.eclipse.xtend.ide.editor.XtendEditorErrorTickUpdater;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;

import io.sarl.lang.ui.internal.LangActivator;

/** Updater of the error ticks into the SARL editor.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8.6
 */
@SuppressWarnings("restriction")
public class SARLEditorErrorTickUpdater extends XtendEditorErrorTickUpdater {

	private static final String MARKER_TYPE = "org.eclipse.xtext.ui.editor.error"; //$NON-NLS-1$

	@Inject
	private IResourceSetProvider resourceSetProvider;

	@Inject
	private IStorage2UriMapper uriMapper;

	@Inject
	private ReflectExtensions reflect;

	/**
	 * Replies the editor that is linked to this updater.
	 *
	 * @return the linked editor, or {@code null} if no editor was linked.
	 * @throws RuntimeException a runtime exception
	 */
	protected XtextEditor getEditor() {
		// FIXME: Remove when Xbase is fixed.
		try {
			return (XtextEditor) this.reflect.get(this, "editor"); //$NON-NLS-1$
		} catch (Exception exception) {
			throw new RuntimeException(exception);
		}
	}

	/**
	 * Replies the current Xtext document that is supported by the linked editor.
	 *
	 * @return the Xtext document, or {@code null} if neither an editor nor a
	 *     Xtext document was linked.
	 */
	protected Resource getXtextResource() {
		//FIXME: for helping to resolve #661
		final var editor = getEditor();
		if (editor != null) {
			final var resource = editor.getResource();
			if (resource instanceof IStorage) {
				final var project = resource.getProject();
				if (project != null) {
					final var resourceSet = this.resourceSetProvider.get(project);
					assert resourceSet != null;
					final var uri = this.uriMapper.getUri((IStorage) resource);
					if (uri != null) {
						return resourceSet.getResource(uri, true);
					}
				}
			}
		}
		return null;
	}

	@Override
	public void modelChanged(final IAnnotationModel model) {
		super.modelChanged(model);
		//FIXME: for helping to resolve #661
		final var display = PlatformUI.getWorkbench().getDisplay();
		display.asyncExec(() -> {
			final var editor = getEditor();
			if (editor != null && !editor.isDirty() && editor.getInternalSourceViewer() != null) {
				final var currentModel = editor.getInternalSourceViewer().getAnnotationModel();
				if (currentModel != null && currentModel == model) {
					final var resource = getXtextResource();
					if (isReconciliable(resource)) {
						final var markers = extractErrorMarkerMessages(currentModel);
						final var resourceErrors = resource.getErrors();
						if (markers.size() != resourceErrors.size() || notSame(markers, resourceErrors)) {
							LangActivator.getInstance().getLog().log(
									new Status(IStatus.ERROR, LangActivator.PLUGIN_ID,
									MessageFormat.format(Messages.SARLEditorErrorTickUpdater_0, resource.getURI())));
						}
					}
				}
			}
		});
	}

	private static boolean isReconciliable(Resource resource) {
		//FIXME: for helping to resolve #661
		if (resource != null) {
			return true;
		}
		return false;
	}

	private static boolean notSame(Set<String> markers, List<Diagnostic> diagnostics) {
		//FIXME: for helping to resolve #661
		for (final var diag : diagnostics) {
			if (!markers.contains(diag.getMessage())) {
				return true;
			}
		}
		return false;
	}

	private static Set<String> extractErrorMarkerMessages(IAnnotationModel currentModel) {
		//FIXME: for helping to resolve #661
		final var annotations = currentModel.getAnnotationIterator();
		final var errors = new TreeSet<String>();
		while (annotations.hasNext()) {
			final var annotation = annotations.next();
			if (annotation != null && !annotation.isMarkedDeleted() && MARKER_TYPE.equals(annotation.getType())) {
				final var text = annotation.getText();
				if (!Strings.isNullOrEmpty(text)) {
					errors.add(text);
				}
			}
		}
		return errors;
	}

}
