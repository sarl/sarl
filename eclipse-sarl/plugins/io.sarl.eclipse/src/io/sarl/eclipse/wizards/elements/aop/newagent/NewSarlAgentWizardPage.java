/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.eclipse.wizards.elements.aop.newagent;

import javax.inject.Inject;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.wizards.elements.AbstractNewSarlElementWizardPage;
import io.sarl.eclipse.wizards.elements.AbstractSuperTypeSelectionDialog;
import io.sarl.eclipse.wizards.elements.SarlSpecificTypeSelectionExtension;
import io.sarl.lang.codebuilder.appenders.ScriptSourceAppender;
import io.sarl.lang.codebuilder.builders.ISarlAgentBuilder;
import io.sarl.lang.core.Agent;

/**
 * Wizard page for creating a new SARL agent.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlAgentWizardPage extends AbstractNewSarlElementWizardPage {

	/** Construct a wizard page.
	 */
	public NewSarlAgentWizardPage() {
		super(CLASS_TYPE, Messages.NewSarlAgent_0);
		setTitle(Messages.NewSarlAgent_0);
		setDescription(Messages.NewSarlAgentPage_0);
		setImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(SARLEclipseConfig.NEW_AGENT_WIZARD_DIALOG_IMAGE));
	}

	@Override
	public void createPageControls(Composite parent) {
		createSuperClassControls(parent, COLUMNS);
		createSeparator(parent, COLUMNS);
		createMethodStubControls(parent, COLUMNS, false, true);
	}

	@Override
	protected void doStatusUpdate() {
		final IStatus[] status = new IStatus[] {
			this.fContainerStatus,
			this.fPackageStatus,
			this.fTypeNameStatus,
			this.fSuperClassStatus,
		};
		updateStatus(status);
	}
	
	@Inject
	private IResourceSetProvider resourceSetProvider;

	@Override
	protected void generateTypeContent(ISourceAppender appender, IJvmTypeProvider typeProvider,
			IProgressMonitor monitor) throws Exception {
		final SubMonitor mon = SubMonitor.convert(monitor, 3);

		final ScriptSourceAppender scriptBuilder = this.codeBuilderFactory.buildScript(
				getPackageFragment().getElementName(), typeProvider);
		final ISarlAgentBuilder agent = scriptBuilder.addSarlAgent(getTypeName());
		agent.setExtends(getSuperClass());
		mon.worked(1);

		URI uri = URI.createURI("platform:/resource/testcreation/src/main/sarl/test.sarl");
		Resource resource = this.resourceSetProvider.get(getJavaProject().getProject()).getResource(uri, true);
		EObject obj = resource.getContents().get(0);
//		
//		System.out.println(EmfFormatter.objToStr(obj));

		/*
		 SarlScript {
    cref XImportSection importSection XImportSection {
        cref XImportDeclaration importDeclarations [
            0: XImportDeclaration {
                ref JvmDeclaredType importedType ref: JvmGenericType@java:/Objects/io.sarl.lang.core.BuiltinCapacitiesProvider#io.sarl.lang.core.BuiltinCapacitiesProvider
            }
            1: XImportDeclaration {
                ref JvmDeclaredType importedType ref: JvmGenericType@java:/Objects/java.util.UUID#java.util.UUID
            }
            2: XImportDeclaration {
                ref JvmDeclaredType importedType ref: JvmGenericType@platform:/resource/testcreation/src/main/sarl/testcreation2/WizAgent1.sarl#/1
            }
        ]
    }
    cref XtendTypeDeclaration xtendTypes [
        0: SarlAgent {
            cref XtendAnnotationTarget annotationInfo XtendTypeDeclaration {
            }
            attr EString name 'WWW'
            cref XtendMember members [
                0: SarlConstructor {
                    cref XtendAnnotationTarget annotationInfo XtendMember {
                    }
                    ref XtendTypeDeclaration declaringType ref: SarlAgent@/0/@xtendTypes.0
                    cref XExpression expression XBlockExpression {
                        cref XExpression expressions [
                            0: XFeatureCall {
                                ref JvmIdentifiableElement feature ref: JvmConstructor@platform:/resource/testcreation/src/main/sarl/testcreation2/WizAgent1.sarl#/1/@members.0
                                cref XExpression featureCallArguments [
                                    0: XFeatureCall {
                                        ref JvmIdentifiableElement feature ref: JvmFormalParameter@/1/@members.0/@parameters.0
                                    }
                                    1: XFeatureCall {
                                        ref JvmIdentifiableElement feature ref: JvmFormalParameter@/1/@members.0/@parameters.1
                                    }
                                    2: XFeatureCall {
                                        ref JvmIdentifiableElement feature ref: JvmFormalParameter@/1/@members.0/@parameters.2
                                    }
                                    3: XFeatureCall {
                                        ref JvmIdentifiableElement feature ref: JvmFormalParameter@/1/@members.0/@parameters.3
                                    }
                                ]
                                attr EBoolean explicitOperationCall 'true'
                            }
                        ]
                    }
                    cref XtendParameter parameters [
                        0: SarlFormalParameter {
                            attr EString name 'a'
                            cref JvmTypeReference parameterType JvmParameterizedTypeReference {
                                ref JvmType type ref: JvmGenericType@java:/Objects/io.sarl.lang.core.BuiltinCapacitiesProvider#io.sarl.lang.core.BuiltinCapacitiesProvider
                            }
                        }
                        1: SarlFormalParameter {
                            attr EString name 'b'
                            cref JvmTypeReference parameterType JvmParameterizedTypeReference {
                                ref JvmType type ref: JvmGenericType@java:/Objects/java.util.UUID#java.util.UUID
                            }
                        }
                        2: SarlFormalParameter {
                            attr EString name 'c'
                            cref JvmTypeReference parameterType JvmParameterizedTypeReference {
                                ref JvmType type ref: JvmGenericType@java:/Objects/java.util.UUID#java.util.UUID
                            }
                        }
                        3: SarlFormalParameter {
                            attr EString name 'd'
                            cref JvmTypeReference parameterType JvmParameterizedTypeReference {
                                ref JvmType type ref: JvmPrimitiveType@java:/Primitives#int
                            }
                        }
                    ]
                }
            ]
            cref JvmParameterizedTypeReference extends JvmParameterizedTypeReference {
                ref JvmType type ref: JvmGenericType@platform:/resource/testcreation/src/main/sarl/testcreation2/WizAgent1.sarl#/1
            }
        }
    ]
    attr EString package 'toto'
}

		 */

		if (agent.getSarlAgent().getExtends() != null) {
			createInheritedMembers(
					Agent.class.getCanonicalName(), getSuperClass(),
					agent.getSarlAgent(),
					() -> agent.addSarlConstructor(),
					(name) -> agent.addSarlAction(name));
		}
		mon.worked(2);

		System.out.println(EmfFormatter.objToStr(scriptBuilder.getScript()));
/*
 SarlScript {
    cref XtendTypeDeclaration xtendTypes [
        0: SarlAgent {
            cref XtendAnnotationTarget annotationInfo XtendTypeDeclaration {
            }
            attr EString name 'WizAgent3'
            cref XtendMember members [
                0: SarlConstructor {
                    cref XtendAnnotationTarget annotationInfo XtendTypeDeclaration {
                    }
                    ref XtendTypeDeclaration declaringType ref: SarlAgent@/0/@xtendTypes.0
                    cref XExpression expression XBlockExpression {
                        cref XExpression expressions [
                            0: XBlockExpression {
                            }
                        ]
                    }
                }
            ]
            cref JvmParameterizedTypeReference extends JvmParameterizedTypeReference {
                ref JvmType type ref: JvmGenericType@platform:/resource/testcreation/src/main/sarl/testcreation2/WizAgent1.sarl#/1
            }
        }
    ]
    attr EString package 'testcreation2'
}
		
 */
		scriptBuilder.build(appender);
		mon.done();
	}

	@Override
	protected String getExistingElementErrorMessage() {
		return Messages.NewSarlAgentWizardPage_1;
	}

	@Override
	protected String getInvalidSubtypeErrorMessage() {
		return Messages.NewSarlAgentWizardPage_2;
	}

	@Override
	protected IType getRootSuperType() throws JavaModelException {
		return getJavaProject().findType(Agent.class.getName());
	}

	@Override
	protected AbstractSuperTypeSelectionDialog<?> createSuperClassSelectionDialog(Shell parent,
			IRunnableContext context, IJavaProject project, SarlSpecificTypeSelectionExtension extension,
			boolean multi) {
		return new SuperAgentSelectionDialog(parent, context, project, this, extension, multi);
	}

}
