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

package io.sarl.eclipse.wizards.sreinstall;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.stream.Collectors;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.jface.wizard.Wizard;

import io.sarl.apputils.eclipseextensions.sreinstall.AbstractSREInstallPage;
import io.sarl.apputils.eclipseextensions.sreinstall.SREInstallPages;
import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.runtime.ManifestBasedSREInstall;
import io.sarl.eclipse.runtime.SREException;

/**
 * Wiazrd for SRE installation.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class SREInstallWizard extends Wizard {

	private ISREInstall sre;

	private String[] names;

	/**
	 * Constructs a new wizard to add/edit a SRE.
	 *
	 * @param editSRE the SRE being edited, or {@code null} if none.
	 * @param currentInstalls current SRE installs used to validate name changes.
	 */
	public SREInstallWizard(ISREInstall editSRE, ISREInstall[] currentInstalls) {
		this.sre = editSRE;
		final var names = new ArrayList<String>(currentInstalls.length);
		for (var i = 0; i < currentInstalls.length; i++) {
			final var install = currentInstalls[i];
			if (this.sre == null || !install.getId().equals(this.sre.getId())) {
				final var name = install.getNameNoDefault();
				if (!Strings.isNullOrEmpty(name) && !name.equals(install.getId())) {
					names.add(name);
				}
			}
		}
		this.names = names.toArray(new String[names.size()]);
	}

	/**
	 * Returns the original SRE to edit, or {@code null} if creating a SRE.
	 *
	 * @return SRE to edit or {@code null}
	 */
	protected ISREInstall getOriginalSRE() {
		return this.sre;
	}

	@Override
	public boolean performFinish() {
		return true;
	}

	/**
	 * Returns a page to use for editing a SRE install type.
	 *
	 * @param sre the edited SRE.
	 * @return the wizard page.
	 */
	public AbstractSREInstallPage getPage(ISREInstall sre) {
		final var extensions = SREInstallPages.getSREInstallStreamFromExtension()
			.collect(Collectors.toList());

		IConfigurationElement firstTypeMatching = null;
		for (final var info : extensions) {
			final var id = info.getAttribute("sreInstallId"); //$NON-NLS-1$
			if (sre.getId().equals(Strings.nullToEmpty(id))) {
				try {
					final var page = (AbstractSREInstallPage)
							info.createExecutableExtension("class"); //$NON-NLS-1$
					page.setExistingNames(this.names);
					return page;
				} catch (CoreException e) {
					SARLEclipsePlugin.getDefault().log(e);
				}
			} else if (firstTypeMatching == null
					&& isInstance(info.getAttribute("sreInstallType"), sre)) { //$NON-NLS-1$
				firstTypeMatching = info;
			}
		}

		if (firstTypeMatching != null) {
			try {
				final var page = (AbstractSREInstallPage)
						firstTypeMatching.createExecutableExtension("class"); //$NON-NLS-1$
				page.setExistingNames(this.names);
				return page;
			} catch (CoreException e) {
				SARLEclipsePlugin.getDefault().log(e);
			}
		}
		
		if (sre == null || sre instanceof ManifestBasedSREInstall) {
			final var standardVMPage = new StandardSREPage();
			standardVMPage.setExistingNames(this.names);
			return standardVMPage;
		}

		throw new SREException(MessageFormat.format(
				Messages.SREInstallWizard_5, sre.getName()));
	}

	private static boolean isInstance(String classname, ISREInstall sre) {
		if (!Strings.isNullOrEmpty(classname)) {
			Class<?> type = sre.getClass();
			while (type != null && !Object.class.equals(type)) {
				if (classname.equals(type.getName())) {
					return true;
				}
				type = type.getSuperclass();
			}
		}
		return false;
	}

}
