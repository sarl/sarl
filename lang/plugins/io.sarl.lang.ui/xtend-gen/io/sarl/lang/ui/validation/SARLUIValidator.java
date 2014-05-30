/**
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

import com.google.inject.Inject;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.util.Pair;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.ui.validation.XbaseUIValidator;

/**
 * Validator based on the Eclipse UI.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLUIValidator extends XbaseUIValidator {
  @Inject
  private Logger log;
  
  @Inject
  private IStorage2UriMapper storage2UriMapper;
  
  protected List<EPackage> getEPackages() {
    List<EPackage> packages = super.getEPackages();
    packages.add(SarlPackage.eINSTANCE);
    EPackage _ePackage = EPackage.Registry.INSTANCE.getEPackage("http://www.eclipse.org/xtext/common/JavaVMTypes");
    packages.add(_ePackage);
    EPackage _ePackage_1 = EPackage.Registry.INSTANCE.getEPackage("http://www.eclipse.org/xtext/xbase/Xbase");
    packages.add(_ePackage_1);
    EPackage _ePackage_2 = EPackage.Registry.INSTANCE.getEPackage("http://www.eclipse.org/xtext/xbase/Xtype");
    packages.add(_ePackage_2);
    return packages;
  }
  
  @Check
  public void checkFileNamingConventions(final SarlScript sarlFile) {
    boolean _isIgnored = this.isIgnored(IssueCodes.WRONG_PACKAGE);
    boolean _not = (!_isIgnored);
    if (_not) {
      String expectedPackage = this.getExpectedPackageName(sarlFile);
      String declaredPackage = sarlFile.getName();
      boolean _and = false;
      boolean _tripleNotEquals = (expectedPackage != null);
      if (!_tripleNotEquals) {
        _and = false;
      } else {
        boolean _or = false;
        boolean _and_1 = false;
        boolean _isEmpty = Strings.isEmpty(expectedPackage);
        if (!_isEmpty) {
          _and_1 = false;
        } else {
          boolean _tripleEquals = (declaredPackage == null);
          _and_1 = _tripleEquals;
        }
        if (_and_1) {
          _or = true;
        } else {
          boolean _equals = expectedPackage.equals(declaredPackage);
          _or = _equals;
        }
        boolean _not_1 = (!_or);
        _and = _not_1;
      }
      if (_and) {
        String _notNull = Strings.notNull(declaredPackage);
        String _notNull_1 = Strings.notNull(expectedPackage);
        String _format = String.format("The declared package \'%s\' does not match the expected package \'%s\'", _notNull, _notNull_1);
        EObject _currentObject = this.getCurrentObject();
        this.addIssue(_format, _currentObject, 
          SarlPackage.Literals.SARL_SCRIPT__NAME, 
          ValidationMessageAcceptor.INSIGNIFICANT_INDEX, 
          IssueCodes.WRONG_PACKAGE, expectedPackage);
      }
    }
  }
  
  protected String getExpectedPackageName(final SarlScript sarlFile) {
    Resource _eResource = sarlFile.eResource();
    URI fileURI = _eResource.getURI();
    Iterable<Pair<IStorage, IProject>> _storages = this.storage2UriMapper.getStorages(fileURI);
    for (final Pair<IStorage, IProject> storage : _storages) {
      {
        IStorage first = storage.getFirst();
        if ((first instanceof IFile)) {
          IPath fileWorkspacePath = ((IFile)first).getFullPath();
          IProject _second = storage.getSecond();
          IJavaProject javaProject = JavaCore.create(_second);
          boolean _and = false;
          boolean _and_1 = false;
          boolean _tripleNotEquals = (javaProject != null);
          if (!_tripleNotEquals) {
            _and_1 = false;
          } else {
            boolean _exists = javaProject.exists();
            _and_1 = _exists;
          }
          if (!_and_1) {
            _and = false;
          } else {
            boolean _isOpen = javaProject.isOpen();
            _and = _isOpen;
          }
          if (_and) {
            try {
              IPackageFragmentRoot[] _packageFragmentRoots = javaProject.getPackageFragmentRoots();
              for (final IPackageFragmentRoot root : _packageFragmentRoots) {
                boolean _and_2 = false;
                boolean _isArchive = root.isArchive();
                boolean _not = (!_isArchive);
                if (!_not) {
                  _and_2 = false;
                } else {
                  boolean _isExternal = root.isExternal();
                  boolean _not_1 = (!_isExternal);
                  _and_2 = _not_1;
                }
                if (_and_2) {
                  IResource resource = root.getResource();
                  boolean _tripleNotEquals_1 = (resource != null);
                  if (_tripleNotEquals_1) {
                    IPath sourceFolderPath = resource.getFullPath();
                    boolean _isPrefixOf = sourceFolderPath.isPrefixOf(fileWorkspacePath);
                    if (_isPrefixOf) {
                      IPath claspathRelativePath = fileWorkspacePath.makeRelativeTo(sourceFolderPath);
                      IPath _removeLastSegments = claspathRelativePath.removeLastSegments(1);
                      String _string = _removeLastSegments.toString();
                      return _string.replace("/", ".");
                    }
                  }
                }
              }
            } catch (final Throwable _t) {
              if (_t instanceof JavaModelException) {
                final JavaModelException e = (JavaModelException)_t;
                this.log.log(Level.SEVERE, "Error resolving expected path for SarlScript", e);
              } else {
                throw Exceptions.sneakyThrow(_t);
              }
            }
          }
        }
      }
    }
    return null;
  }
}
