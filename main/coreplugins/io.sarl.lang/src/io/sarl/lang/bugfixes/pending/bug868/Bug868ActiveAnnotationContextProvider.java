/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.bugfixes.pending.bug868;

import java.util.List;
import java.util.function.Consumer;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.apache.log4j.Logger;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend.core.macro.ActiveAnnotationContext;
import org.eclipse.xtend.core.macro.ActiveAnnotationContextProvider;
import org.eclipse.xtend.core.macro.ActiveAnnotationContexts;
import org.eclipse.xtend.core.macro.ProcessorInstanceForJvmTypeProvider;
import org.eclipse.xtend.core.macro.XAnnotationExtensions;
import org.eclipse.xtend.core.macro.declaration.CompilationUnitImpl;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtend.core.xtend.XtendAnnotationTarget;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.service.OperationCanceledManager;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.util.internal.Stopwatches;
import org.eclipse.xtext.validation.EObjectDiagnosticImpl;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;

/** Provide the contexts of the active annotations.
 *
 * <p>FIXME: Remove this class when <a href="https://github.com/eclipse/xtext-xtend/pull/649">PR 649</a> is merged.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 * @see "https://github.com/eclipse/xtext-xtend/pull/649"
 */
@SuppressWarnings("all")
public class Bug868ActiveAnnotationContextProvider extends ActiveAnnotationContextProvider {
	private final static Logger logger = Logger.getLogger(ActiveAnnotationContextProvider.class);

	@Inject
	@Extension
	private XAnnotationExtensions _xAnnotationExtensions;

	@Inject
	@Extension
	private ProcessorInstanceForJvmTypeProvider _processorInstanceForJvmTypeProvider;

	@Inject
	private Provider<CompilationUnitImpl> compilationUnitProvider;

	@Inject
	private OperationCanceledManager operationCanceledManager;

	@Inject
	private ReflectExtensions reflect;

	public ActiveAnnotationContexts computeContext(final XtendFile file) {
		final Stopwatches.StoppedTask task = Stopwatches.forTask("[macros] findActiveAnnotations (ActiveAnnotationContextProvider.computeContext)");
		task.start();
		try {
			final ActiveAnnotationContexts result = ActiveAnnotationContexts.installNew(file.eResource());
			final CompilationUnitImpl compilationUnit = this.compilationUnitProvider.get();
			compilationUnit.setXtendFile(file);
			this.reflect.set(result, "compilationUnit", compilationUnit);
			final IAcceptor<Pair<JvmAnnotationType, XAnnotation>> _function = (Pair<JvmAnnotationType, XAnnotation> it) -> {
				boolean _containsKey = result.getContexts().containsKey(it.getKey());
				boolean _not = (!_containsKey);
				if (_not) {
					final ActiveAnnotationContext fa = new ActiveAnnotationContext();
					fa.setCompilationUnit(compilationUnit);
					final JvmType processorType = this._xAnnotationExtensions.getProcessorType(it.getKey());
					try {
						final Object processorInstance = this._processorInstanceForJvmTypeProvider.getProcessorInstance(processorType);
						if ((processorInstance == null)) {
							String _identifier = processorType.getIdentifier();
							String _plus = ("Couldn\'t instantiate the annotation processor of type \'" + _identifier);
							String _plus_1 = (_plus + "\'. This is usually the case when the processor resides in the same project as the annotated element.");
							throw new IllegalStateException(_plus_1);
						}
						fa.setProcessorInstance(processorInstance);
					} catch (final Throwable _t) {
						if (_t instanceof VirtualMachineError) {
							final VirtualMachineError e = (VirtualMachineError)_t;
							throw e;
						} else if (_t instanceof Throwable) {
							final Throwable e_1 = (Throwable)_t;
							this.operationCanceledManager.propagateAsErrorIfCancelException(e_1);
							String _switchResult = null;
							boolean _matched = false;
							if (e_1 instanceof ExceptionInInitializerError) {
								_matched=true;
								_switchResult = ((ExceptionInInitializerError)e_1).getException().getMessage();
							}
							if (!_matched) {
								_switchResult = e_1.getMessage();
							}
							final String msg = _switchResult;
							EList<Resource.Diagnostic> _errors = file.eResource().getErrors();
							StringConcatenation _builder = new StringConcatenation();
							_builder.append("Problem while loading annotation processor: ");
							_builder.append(msg);
							XAnnotation _value = it.getValue();
							EObjectDiagnosticImpl _eObjectDiagnosticImpl = new EObjectDiagnosticImpl(Severity.ERROR, 
									IssueCodes.PROCESSING_ERROR, _builder.toString(), _value, 
									XAnnotationsPackage.Literals.XANNOTATION__ANNOTATION_TYPE, (-1), null);
							_errors.add(_eObjectDiagnosticImpl);
						} else {
							throw Exceptions.sneakyThrow(_t);
						}
					}
					result.getContexts().put(it.getKey(), fa);
				}
				List<XtendAnnotationTarget> _annotatedSourceElements = result.getContexts().get(it.getKey()).getAnnotatedSourceElements();
				XtendAnnotationTarget _annotatedTarget = this._xAnnotationExtensions.getAnnotatedTarget(it.getValue());
				_annotatedSourceElements.add(_annotatedTarget);
			};
			this.searchAnnotatedElements(file, _function);
			return result;
		} catch (final Throwable _t) {
			if (_t instanceof Throwable) {
				final Throwable e = (Throwable)_t;
				this.operationCanceledManager.propagateAsErrorIfCancelException(e);
				boolean _matched = false;
				if (e instanceof VirtualMachineError) {
					_matched=true;
					throw ((VirtualMachineError)e);
				}
				if (!_matched) {
					if (e instanceof LinkageError) {
						_matched=true;
						throw ((LinkageError)e);
					}
				}
				//ActiveAnnotationContextProvider.logger.warn("Error finding the elements to be processed by active annotations", e);
				return ActiveAnnotationContexts.installNew(file.eResource());
			} else {
				throw Exceptions.sneakyThrow(_t);
			}
		} finally {
			task.stop();
		}
	}

	/**
	 * recursively looks for macro annotations on XtendAnnotationTargets
	 */
	protected void searchAnnotatedElements(final EObject element, final IAcceptor<Pair<JvmAnnotationType, XAnnotation>> acceptor) {
		boolean _matched = false;
		if (element instanceof XtendFile) {
			_matched=true;
			final Consumer<XtendTypeDeclaration> _function = (XtendTypeDeclaration it) -> {
				this.searchAnnotatedElements(it, acceptor);
			};
			((XtendFile)element).getXtendTypes().forEach(_function);
		}
		if (!_matched) {
			if (element instanceof XtendClass) {
				_matched=true;
				this.registerMacroAnnotations(((XtendAnnotationTarget)element), acceptor);
				final Consumer<XtendMember> _function = (XtendMember it) -> {
					this.searchAnnotatedElements(it, acceptor);
				};
				((XtendClass)element).getMembers().forEach(_function);
			}
		}
		if (!_matched) {
			if (element instanceof XtendInterface) {
				_matched=true;
				this.registerMacroAnnotations(((XtendAnnotationTarget)element), acceptor);
				final Consumer<XtendMember> _function = (XtendMember it) -> {
					this.searchAnnotatedElements(it, acceptor);
				};
				((XtendInterface)element).getMembers().forEach(_function);
			}
		}
		if (!_matched) {
			if (element instanceof XtendEnum) {
				_matched=true;
				this.registerMacroAnnotations(((XtendAnnotationTarget)element), acceptor);
				final Consumer<XtendMember> _function = (XtendMember it) -> {
					this.searchAnnotatedElements(it, acceptor);
				};
				((XtendEnum)element).getMembers().forEach(_function);
			}
		}
		if (!_matched) {
			if (element instanceof XtendAnnotationType) {
				_matched=true;
				this.registerMacroAnnotations(((XtendAnnotationTarget)element), acceptor);
				final Consumer<XtendMember> _function = (XtendMember it) -> {
					this.searchAnnotatedElements(it, acceptor);
				};
				((XtendAnnotationType)element).getMembers().forEach(_function);
			}
		}
		if (!_matched) {
			if (element instanceof XtendFunction) {
				_matched=true;
				this.registerMacroAnnotations(((XtendAnnotationTarget)element), acceptor);
				final Consumer<XtendParameter> _function = (XtendParameter it) -> {
					this.searchAnnotatedElements(it, acceptor);
				};
				((XtendFunction)element).getParameters().forEach(_function);
			}
		}
		if (!_matched) {
			if (element instanceof XtendConstructor) {
				_matched=true;
				this.registerMacroAnnotations(((XtendAnnotationTarget)element), acceptor);
				final Consumer<XtendParameter> _function = (XtendParameter it) -> {
					this.searchAnnotatedElements(it, acceptor);
				};
				((XtendConstructor)element).getParameters().forEach(_function);
			}
		}
		if (!_matched) {
			if (element instanceof XtendAnnotationTarget) {
				_matched=true;
				this.registerMacroAnnotations(((XtendAnnotationTarget)element), acceptor);
			}
		}
	}

	protected void registerMacroAnnotations(final XtendAnnotationTarget candidate, final IAcceptor<Pair<JvmAnnotationType, XAnnotation>> acceptor) {
		final Function1<XAnnotation, Boolean> _function = (XAnnotation it) -> {
			return Boolean.valueOf(this._xAnnotationExtensions.isProcessed(it));
		};
		Iterable<XAnnotation> _filter = IterableExtensions.<XAnnotation>filter(candidate.getAnnotations(), _function);
		for (final XAnnotation annotation : _filter) {
			{
				final JvmAnnotationType activeAnnotationDeclaration = this._xAnnotationExtensions.tryFindAnnotationType(annotation);
				if ((activeAnnotationDeclaration != null)) {
					boolean _isValid = this.isValid(annotation, activeAnnotationDeclaration);
					if (_isValid) {
						Pair<JvmAnnotationType, XAnnotation> _mappedTo = Pair.<JvmAnnotationType, XAnnotation>of(activeAnnotationDeclaration, annotation);
						acceptor.accept(_mappedTo);
					}
				}
			}
		}
	}

	private boolean isValid(final XAnnotation annotation, final JvmAnnotationType activeAnnotationDeclaration) {
		return (annotation != null);
	}

}
