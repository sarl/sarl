/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.ui.validation;

import com.google.inject.Inject
import io.sarl.lang.sarl.SarlPackage
import io.sarl.lang.sarl.SarlScript
import io.sarl.lang.validation.IssueCodes
import java.util.List
import java.util.logging.Logger
import org.eclipse.emf.ecore.EPackage
import org.eclipse.xtext.ui.resource.IStorage2UriMapper
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.eclipse.xtext.xbase.ui.validation.XbaseUIValidator

import static org.eclipse.xtext.util.Strings.*
import org.eclipse.core.resources.IFile
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.JavaModelException
import java.util.logging.Level

/** Validator based on the Eclipse UI.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLUIValidator extends XbaseUIValidator {

	@Inject
	private Logger log

	@Inject 
	private IStorage2UriMapper storage2UriMapper

	protected override List<EPackage> getEPackages() {
		var packages = super.getEPackages()
		packages.add(SarlPackage.eINSTANCE)
	    packages.add(EPackage.Registry.INSTANCE.getEPackage("http://www.eclipse.org/xtext/common/JavaVMTypes"))
	    packages.add(EPackage.Registry.INSTANCE.getEPackage("http://www.eclipse.org/xtext/xbase/Xbase"))
	    packages.add(EPackage.Registry.INSTANCE.getEPackage("http://www.eclipse.org/xtext/xbase/Xtype"))
		return packages;
	}


	@Check
	public def checkFileNamingConventions(SarlScript sarlFile) {
		if (!isIgnored(IssueCodes.WRONG_PACKAGE)) {
			var expectedPackage = sarlFile.getExpectedPackageName
			var declaredPackage = sarlFile.name
			if(expectedPackage !== null && 
				!((isEmpty(expectedPackage) && declaredPackage === null) || 
					expectedPackage.equals(declaredPackage))) {
				addIssue(
					String.format("The declared package '%s' does not match the expected package '%s'",
						notNull(declaredPackage),
						notNull(expectedPackage)
					),
					currentObject,
					SarlPackage.Literals::SARL_SCRIPT__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					IssueCodes.WRONG_PACKAGE,
					expectedPackage)
			}
		}
	}
	
	/** Replies the expected package name for a SARL script.
	 * 
	 * @param sarlFile - script to consider.
	 * @param storage2UriMapper - mapper from storage to URI.
	 * @param log - logger to use for output the problems. It may be <code>null</code>.
	 * @return the expected package name.
	 */
	protected def String getExpectedPackageName(SarlScript sarlFile) {
		var fileURI = sarlFile.eResource.URI
		for(storage : storage2UriMapper.getStorages(fileURI)) {
			var first = storage.first
			if (first instanceof IFile) {
				var fileWorkspacePath = first.fullPath
				var javaProject = JavaCore::create(storage.second)
				if(javaProject!==null && javaProject.exists && javaProject.isOpen) {
					try {
						for(root: javaProject.packageFragmentRoots) {
							if(!root.isArchive && !root.isExternal) {
								var resource = root.resource
								if(resource!==null) {
									var sourceFolderPath = resource.fullPath
									if(sourceFolderPath.isPrefixOf(fileWorkspacePath)) {
										var claspathRelativePath = fileWorkspacePath.makeRelativeTo(sourceFolderPath)
										return claspathRelativePath.removeLastSegments(1).toString().replace("/", ".")
									}
								}
							}
						}
					}
					catch(JavaModelException e) {
						log.log(Level::SEVERE, "Error resolving expected path for SarlScript", e); //$NON-NLS-1$
					}
				}
			}
		}
		return null;
	}

}
