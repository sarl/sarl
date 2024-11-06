/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import java.util.List;
import java.util.stream.Collectors;

import com.google.inject.Inject;
import org.eclipse.xtend.core.compiler.XtendGenerator;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.DisableCodeGenerationAdapter;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.SARLConfig;
import io.sarl.lang.typesystem.IOperationHelper;
import io.sarl.lang.util.Utils;

/** SARL-specific generator.
 *
 * <p>This generator:<ul>
 * <li>forces the JvmOperation to be annotated with {@code @Pure}
 * dynamically.</li>
 * <li>Generate static initialization block from a static constructor;</li>
 * <li>Generate the files into the standard output folder or the unit test output folder.</li>
 * </ul>
 *
 * <p>The roles of the different generation tools are:<ul>
 * <li>{@link io.sarl.lang.jvmmodel.SARLJvmModelInferrer}: Generating the expected Java Ecore model from the SARL Ecore model.</li>
 * <li>{@link org.eclipse.xtext.linking.ILinker}: Create links among the SARL Ecore models.<li>
 * <li>{@link SARLJvmGenerator}: Generate the Java code from the Java Ecore model.</li>
 * <li>{@link io.sarl.lang.compiler.SarlCompiler}: Generate the Java code for the XExpression objects.</li>
 * </ul>
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.4
 */
public class SARLJvmGenerator extends XtendGenerator {

	@Inject
	private IOperationHelper operationHelper;

	@Inject
	private AnnotationLookup annotations;

	@Inject
	private IGeneratorConfigProvider generatorConfigProvider;

	@Inject
	private IResourceTypeDetector resourceTypeDetector;

	@Override
	protected ITreeAppendable _generateMember(JvmOperation it, ITreeAppendable appendable, GeneratorConfig config) {
		if (Utils.isStaticConstructorName(it.getSimpleName())) {
			// The constructor name is not the same as the declaring type.
			// We assume that the constructor is a static constructor.
			return generateStaticConstructor(it, appendable, config);
		}
		// The code below is adapted from the code of the Xtend super type.
		appendable.newLine();
		appendable.openScope();
		generateJavaDoc(it, appendable, config);
		final var tracedAppendable = appendable.trace(it);
		generateAnnotations(it.getAnnotations(), tracedAppendable, true, config);
		// Specific case: automatic generation
		if (this.operationHelper.isPureOperation(it)
				&& this.annotations.findAnnotation(it, Pure.class) == null) {
			tracedAppendable.append("@").append(Pure.class).newLine(); //$NON-NLS-1$
		}
		generateModifier(it, tracedAppendable, config);
		generateTypeParameterDeclaration(it, tracedAppendable, config);
		if (it.getReturnType() == null) {
			tracedAppendable.append("void"); //$NON-NLS-1$
		} else {
			this._errorSafeExtensions.serializeSafely(it.getReturnType(), Object.class.getSimpleName(), tracedAppendable);
		}
		tracedAppendable.append(" "); //$NON-NLS-1$
		this._treeAppendableUtil.traceSignificant(tracedAppendable, it).append(makeJavaIdentifier(it.getSimpleName()));
		tracedAppendable.append("("); //$NON-NLS-1$
		generateParameters(it, tracedAppendable, config);
		tracedAppendable.append(")"); //$NON-NLS-1$
		generateThrowsClause(it, tracedAppendable, config);
		if (it.isAbstract() || !hasBody(it)) {
			tracedAppendable.append(";"); //$NON-NLS-1$
		} else {
			tracedAppendable.append(" "); //$NON-NLS-1$
			generateExecutableBody(it, tracedAppendable, config);
		}
		appendable.closeScope();
		return appendable;
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
		final var tracedAppendable = appendable.trace(it);
		tracedAppendable.append("static "); //$NON-NLS-1$
		generateExecutableBody(it, tracedAppendable, config);
		return appendable;
	}

	@Override
	protected void _internalDoGenerate(JvmDeclaredType type, IFileSystemAccess fsa) {
		if (DisableCodeGenerationAdapter.isDisabled(type)) {
			return;
		}
		final String qn = type.getQualifiedName();
		if (!Strings.isEmpty(qn)) {
			final var content = generateType(type, this.generatorConfigProvider.get(type));
			final var isTest = this.resourceTypeDetector.isTestResource(type.eResource());
			final String outputConfigurationName;
			if (isTest != null && isTest.booleanValue()) {
				outputConfigurationName = SARLConfig.TEST_OUTPUT_CONFIGURATION;
			} else {
				outputConfigurationName = IFileSystemAccess.DEFAULT_OUTPUT;
			}
			final var fn = qn.replace('.', '/') + ".java"; //$NON-NLS-1$
			fsa.generateFile(fn, outputConfigurationName, content);
		}
	}

	@Override
	protected ImportManager createImportManager(JvmDeclaredType type) {
		return new NoDefaultPackageImportManager(type);
	}

	/** Import manager that is skipping the imports for types in default package.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version compiler 0.14.0 20241106-161406
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 * @since 0.13
	 */
	public static class NoDefaultPackageImportManager extends ImportManager {

		private static final String PACKAGE_SEPARATOR = "."; //$NON-NLS-1$

		/** Constructor.
		 *
		 * @param type the type for which the import manager is created.
		 */
		public NoDefaultPackageImportManager(JvmDeclaredType type) {
			super(true, type);
		}

		@Override
		public List<String> getImports() {
			return super.getImports().stream().filter(it -> isPackaged(it)).collect(Collectors.toList());
		}

		private static boolean isPackaged(String name) {
			return name.contains(PACKAGE_SEPARATOR);
		}

	}

}
