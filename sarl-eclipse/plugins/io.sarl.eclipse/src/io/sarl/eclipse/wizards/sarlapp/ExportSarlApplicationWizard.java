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

package io.sarl.eclipse.wizards.sarlapp;

import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.jdt.ui.jarpackager.JarPackageData;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;

/**
 * SARL wizard for export a SARL application into a single Jar file.
 * Most part of the code of this class comes from {@code JavaProjectWizard}.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class ExportSarlApplicationWizard extends FixedFatJarExportWizard {

	@Inject
	private ReflectExtensions reflect;

	@Inject
	private Injector injector;

	/** Construct a new wizard for exporting a SARL application within a Jar file.
	 */
	public ExportSarlApplicationWizard() {
	}

	/** Replies the data that describe the package.
	 *
	 * @return the package data.
	 * @throws RuntimeException a runtime exception
	 */
	protected JarPackageData getJarPackageData() {
		try {
			return (JarPackageData) this.reflect.get(this, "fJarPackage"); //$NON-NLS-1$
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/** Replies the structured selection.
	 *
	 * @return the selection.
	 * @throws RuntimeException a runtime exception
	 */
	protected IStructuredSelection getStructuredSelection() {
		try {
			return (IStructuredSelection) this.reflect.get(this, "fSelection"); //$NON-NLS-1$
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	protected FixedFatJarExportPage createPageInstance(JarPackageData jarPackage, IStructuredSelection selection) {
		final var page = new ExportSarlApplicationPage(jarPackage, selection);
		this.injector.injectMembers(page);
		return page;
	}

	@Override
	public boolean performFinish() {
		return super.performFinish();
	}

}
