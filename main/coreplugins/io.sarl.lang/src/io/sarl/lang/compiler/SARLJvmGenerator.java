/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.compiler;

import javax.inject.Inject;

import org.eclipse.xtend.core.compiler.XtendGenerator;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.linking.ILinker;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;

import io.sarl.lang.jvmmodel.SARLJvmModelInferrer;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.IOperationHelper;
import io.sarl.lang.util.Utils;


/** SARL-specific generator.
 *
 * <p>This generator:<ul>
 * <li>forces the JvmOperation to be annotated with <code>@Pure</code>
 * dynamically.</li>
 * <li>Generate static initialization block from a static constructor.</li>
 * </ul>
 *
 * <p>The roles of the different generation tools are:<ul>
 * <li>{@link SARLJvmModelInferrer}: Generating the expected Java Ecore model from the SARL Ecore model.</li>
 * <li>{@link ILinker}: Create links among the SARL Ecore models.<li>
 * <li>{@link SARLJvmGenerator}: Generate the Java code from the Java Ecore model.</li>
 * <li>{@link SarlCompiler}: Generate the Java code for the XExpression objects.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.4
 */
public class SARLJvmGenerator extends XtendGenerator {

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Inject
	private IOperationHelper operationHelper;

	@Override
	public ITreeAppendable generateMembersInBody(JvmDeclaredType it, ITreeAppendable appendable,
			GeneratorConfig config) {
		for (final JvmMember member : getMembersToBeCompiled(it)) {
			if (member instanceof JvmOperation) {
				// Add the @Pure annotation dynamically
				this.operationHelper.adaptIfPossible((JvmOperation) member);
			}
		}
		return super.generateMembersInBody(it, appendable, config);
	}

	@Override
	protected ITreeAppendable _generateMember(JvmOperation it, ITreeAppendable appendable, GeneratorConfig config) {
		//System.err.println("Generates: <" + it.getSimpleName() + ">");
		if (Utils.STATIC_CONSTRUCTOR_NAME.equals(it.getSimpleName())) {
			// The constructor name is not the same as the declaring type.
			// We assume that the constructor is a static constructor.
			return generateStaticConstructor(it, appendable, config);
		}
		return super._generateMember(it, appendable, config);
	}

	/** Generate a static constructor from the given Jvm constructor.
	 *
	 * @param it the container of the code.
	 * @param appendable the output.
	 * @param config the generation configuration.
	 * @return the appendable.
	 */
	protected ITreeAppendable generateStaticConstructor(JvmOperation it, ITreeAppendable appendable, GeneratorConfig config) {
		appendable.newLine();
		appendable.openScope();
		generateJavaDoc(it, appendable, config);
		final ITreeAppendable tracedAppendable = appendable.trace(it);
		tracedAppendable.append(this.keywords.getStaticStaticKeyword()).append(" "); //$NON-NLS-1$
		generateExecutableBody(it, tracedAppendable, config);
		return appendable;
	}

}
