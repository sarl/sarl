/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.ui.validation;

import static org.eclipse.xtext.util.Strings.isEmpty;
import static org.eclipse.xtext.util.Strings.notNull;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;

import java.text.MessageFormat;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.util.Pair;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.ui.validation.XbaseUIValidator;

import com.google.inject.Inject;

/** Validator based on the Eclipse UI.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLUIValidator extends XbaseUIValidator {

	@Inject
	private Logger log;

	@Inject
	private IStorage2UriMapper storage2UriMapper;

	@Override
	protected List<EPackage> getEPackages() {
		List<EPackage> packages = super.getEPackages();
		packages.add(SarlPackage.eINSTANCE);
		packages.add(EPackage.Registry.INSTANCE.getEPackage("http://www.eclipse.org/xtext/common/JavaVMTypes")); //$NON-NLS-1$
		packages.add(EPackage.Registry.INSTANCE.getEPackage("http://www.eclipse.org/xtext/xbase/Xbase")); //$NON-NLS-1$
		packages.add(EPackage.Registry.INSTANCE.getEPackage("http://www.eclipse.org/xtext/xbase/Xtype")); //$NON-NLS-1$
		return packages;
	}


	/** Check the package name.
	 *
	 * @param sarlFile - the SARL script.
	 */
	@Check
	public void checkFileNamingConventions(SarlScript sarlFile) {
		if (!isIgnored(IssueCodes.WRONG_PACKAGE)) {
			String expectedPackage = getExpectedPackageName(sarlFile);
			String declaredPackage = sarlFile.getName();
			if (expectedPackage != null
					&& !((isEmpty(expectedPackage) && declaredPackage == null)
					  || expectedPackage.equals(declaredPackage))) {
				addIssue(
						MessageFormat.format(Messages.SARLUIValidator_0,
								notNull(declaredPackage),
								notNull(expectedPackage)
								),
								getCurrentObject(),
								SarlPackage.Literals.SARL_SCRIPT__NAME,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								IssueCodes.WRONG_PACKAGE,
								expectedPackage);
			}
		}
	}

	/** Replies the expected package name for a SARL script.
	 *
	 * @param sarlFile - script to consider.
	 * @return the expected package name.
	 */
	protected String getExpectedPackageName(SarlScript sarlFile) {
		URI fileURI = sarlFile.eResource().getURI();
		for (Pair<IStorage, IProject> storage : this.storage2UriMapper.getStorages(fileURI)) {
			IStorage first = storage.getFirst();
			if (first instanceof IFile) {
				IPath fileWorkspacePath = first.getFullPath();
				IJavaProject javaProject = JavaCore.create(storage.getSecond());
				if (javaProject != null && javaProject.exists() && javaProject.isOpen()) {
					try {
						String sourceFolder = getRelativeSourceFolder(javaProject, fileWorkspacePath);
						if (sourceFolder != null) {
							return sourceFolder;
						}
					} catch (JavaModelException e) {
						this.log.log(Level.SEVERE, Messages.SARLUIValidator_1, e);
					}
				}
			}
		}
		return null;
	}

	private static String getRelativeSourceFolder(IJavaProject javaProject, IPath rootPath) throws JavaModelException {
		for (IPackageFragmentRoot root: javaProject.getPackageFragmentRoots()) {
			if (!root.isArchive() && !root.isExternal()) {
				IResource resource = root.getResource();
				if (resource != null) {
					IPath sourceFolderPath = resource.getFullPath();
					if (sourceFolderPath.isPrefixOf(rootPath)) {
						IPath classpathRelativePath = rootPath.makeRelativeTo(sourceFolderPath);
						return classpathRelativePath.removeLastSegments(1).toString().replace(
								"/", "."); //$NON-NLS-1$//$NON-NLS-2$
					}
				}
			}
		}
		return null;
	}

}
