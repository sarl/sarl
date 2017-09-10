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

package io.sarl.lang.compiler.extra;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.inject.Inject;

import com.google.inject.Injector;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.generator.AbstractGenerator;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.generator.IGeneratorContext;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.PolymorphicDispatcher;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.controlflow.IEarlyExitComputer;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeExtensions;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;

/** Abstract implementation for the generator from SARL to an extra language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractExtraLanguageGenerator extends AbstractGenerator implements IRootGenerator {

	private static final String FILENAME_SEPARATOR = "/"; //$NON-NLS-1$

	private final PolymorphicDispatcher<Void> beforeDispatcher;

	private final PolymorphicDispatcher<Void> generateDispatcher;

	private final PolymorphicDispatcher<Void> generateDispatcher2;

	private final PolymorphicDispatcher<Void> afterDispatcher;

	private IQualifiedNameProvider qualifiedNameProvider;

	private IQualifiedNameConverter qualifiedNameConverter;

	private JvmTypesBuilder jvmTypesBuilder;

	private TypeReferences typeReferences;

	private JvmTypeExtensions jvmTypeExtensions;

	private IActionPrototypeProvider actionPrototypeProvider;

	private SarlJvmModelAssociations sarlAssociations;

	private IEarlyExitComputer earlyExitComputer;

	private Injector injector;

	private IBatchTypeResolver typeResolver;

	/** Construct the generator.
	 */
	public AbstractExtraLanguageGenerator() {
		this.beforeDispatcher = new PolymorphicDispatcher<Void>(
				"_before", 2, 2, //$NON-NLS-1$
				Collections.singletonList(this)) {
			@Override
			protected Void handleNoSuchMethod(Object... params) {
				return null;
			}
		};
		this.generateDispatcher = new PolymorphicDispatcher<>(
				"_generate", 2, 2, //$NON-NLS-1$
				Collections.singletonList(this));
		this.generateDispatcher2 = new PolymorphicDispatcher<>(
				"_generate", 3, 3, //$NON-NLS-1$
				Collections.singletonList(this));
		this.afterDispatcher = new PolymorphicDispatcher<Void>(
				"_after", 2, 2, //$NON-NLS-1$
				Collections.singletonList(this)) {
			@Override
			protected Void handleNoSuchMethod(Object... params) {
				return null;
			}
		};
	}

	/** Replies the type converter.
	 *
	 * @param context the context of the generation.
	 * @return the converter.
	 */
	public ExtraLanguageTypeConverter getTypeConverter(IExtraLanguageGeneratorContext context) {
		return getExpressionGenerator().getTypeConverter(context);
	}

	/** Replies the converter of feature names.
	 *
	 * @param context the context of the generation.
	 * @return the converter.
	 */
	public ExtraLanguageFeatureNameConverter getFeatureNameConverter(IExtraLanguageGeneratorContext context) {
		return getExpressionGenerator().getFeatureNameConverter(context);
	}

	/** Change the type reference finder.
	 *
	 * @param finder the type reference finder.
	 */
	@Inject
	public void setTypeReferences(TypeReferences finder) {
		this.typeReferences = finder;
	}

	/** Replies the type reference finder.
	 *
	 * @return the type reference finder.
	 */
	public TypeReferences getTypeReferences() {
		return this.typeReferences;
	}

	/** Change the type resolver for expressions.
	 *
	 * @param resolver the batch type resolver.
	 */
	@Inject
	public void setTypeResolver(IBatchTypeResolver resolver) {
		this.typeResolver = resolver;
	}

	/** Replies the type resolver for expressions.
	 *
	 * @return the batch type resolver.
	 */
	public IBatchTypeResolver getTypeResolver() {
		return this.typeResolver;
	}

	/** Change the injector.
	 *
	 * @param injector the injector.
	 */
	@Inject
	public void setInjector(Injector injector) {
		this.injector = injector;
	}

	/** Replies the injector.
	 *
	 * @return the injector.
	 */
	protected Injector getInjector() {
		return this.injector;
	}

	/** Change the early exit computer.
	 *
	 * @param computer the early exit computer.
	 */
	@Inject
	public void setEarlyExitComputer(IEarlyExitComputer computer) {
		this.earlyExitComputer = computer;
	}

	/** Replies the early exit computer.
	 *
	 * @return the early exit computer.
	 */
	public IEarlyExitComputer getEarlyExitComputer() {
		return this.earlyExitComputer;
	}

	/** Change the associations between the SARL elements and the JVM elements.
	 *
	 * @param associations the associations.
	 */
	@Inject
	public void setJvmModelAssociations(SarlJvmModelAssociations associations) {
		this.sarlAssociations = associations;
	}

	/** Replies the associations between the SARL elements and the JVM elements.
	 *
	 * @return the associations.
	 */
	public SarlJvmModelAssociations getJvmModelAssociations() {
		return this.sarlAssociations;
	}

	/** Change the provider of action prototype.
	 *
	 * @param provider the action prototype provider.
	 */
	@Inject
	public void setLogicalContainerProvider(IActionPrototypeProvider provider) {
		this.actionPrototypeProvider = provider;
	}

	/** Change the provider of action prototype.
	 *
	 * @param provider the action prototype provider.
	 */
	@Inject
	public void setActionPrototypeProvider(IActionPrototypeProvider provider) {
		this.actionPrototypeProvider = provider;
	}

	/** Replies the provider of action prototype.
	 *
	 * @return the action prototype provider.
	 */
	public IActionPrototypeProvider getActionPrototypeProvider() {
		return this.actionPrototypeProvider;
	}

	/** Change the type builder.
	 *
	 * @param builder the builder.
	 */
	@Inject
	public void setTypeBuilder(JvmTypesBuilder builder) {
		this.jvmTypesBuilder = builder;
	}

	/** Replies the type builder.
	 *
	 * @return the builder.
	 */
	public JvmTypesBuilder getTypeBuilder() {
		return this.jvmTypesBuilder;
	}

	/** Change the type extension provider.
	 *
	 * @param provider the type extension provider.
	 */
	@Inject
	public void setTypeExtensions(JvmTypeExtensions provider) {
		this.jvmTypeExtensions = provider;
	}

	/** Replies the type extension provider.
	 *
	 * @return the type extension provider.
	 */
	public JvmTypeExtensions getTypeExtensions() {
		return this.jvmTypeExtensions;
	}

	/** Change the converter of qualified name.
	 *
	 * @param converter the converter.
	 */
	@Inject
	public void setQualifiedNameConverter(IQualifiedNameConverter converter) {
		this.qualifiedNameConverter = converter;
	}

	/** Replies the converter of qualified name.
	 *
	 * @return the converer.
	 */
	public IQualifiedNameConverter getQualifiedNameConverter() {
		return this.qualifiedNameConverter;
	}

	/** Change the provider of qualified name.
	 *
	 * @param provider the provider.
	 */
	@Inject
	public void setQualifiedNameProvider(IQualifiedNameProvider provider) {
		this.qualifiedNameProvider = provider;
	}

	/** Replies the provider of qualified name.
	 *
	 * @return the provider.
	 */
	public IQualifiedNameProvider getQualifiedNameProvider() {
		return this.qualifiedNameProvider;
	}

	/** Replies the expression associated to the given object.
	 * Usually, the expression is inside the given object.
	 *
	 * @param object the object.
	 * @return the expression, or {@code null} if none.
	 */
	protected XExpression getAssociatedExpression(JvmMember object) {
		final XExpression expr = getTypeBuilder().getExpression(object);
		if (expr == null) {
			// The member may be a automatically generated code with dynamic code-building strategies
			final Procedure1<? super ITreeAppendable> strategy = getTypeExtensions().getCompilationStrategy(object);
			if (strategy != null) {
				//
			} else {
				final StringConcatenationClient template = getTypeExtensions().getCompilationTemplate(object);
				if (template != null) {
					//
				}
			}
		}
		return expr;
	}

	/** Replies the generator dedicated to the expressions.
	 *
	 * @return the generator for expressions.
	 */
	public abstract IExpressionGenerator getExpressionGenerator();

	/** Replies the filename for the qualified name.
	 *
	 * @param name the qualified name.
	 * @param separator the filename separator.
	 * @return the filename.
	 */
	protected String toFilename(QualifiedName name, String separator) {
		final List<String> segments = name.getSegments();
		if (segments.isEmpty()) {
			return ""; //$NON-NLS-1$
		}
		final StringBuilder builder = new StringBuilder();
		builder.append(name.toString(separator));
		builder.append(getFilenameExtension());
		return builder.toString();
	}

	/** Replies the filename for the qualified name.
	 *
	 * @param name the qualified name.
	 * @return the filename.
	 */
	protected String toFilename(QualifiedName name) {
		return toFilename(name, FILENAME_SEPARATOR);
	}

	/** Replies the filename extension.
	 *
	 * @return the extension.
	 */
	protected abstract String getFilenameExtension();

	/** Replies the name of the output configuration to be used.
	 *
	 * @return the name of the output configuration.
	 */
	protected abstract String getOutputConfigurationName();

	/** Write the given file.
	 *
	 * @param name the name of the type to write.
	 * @param appendable the content to be written.
	 * @param context the generator context.
	 * @return {@code true} if the file was written.
	 */
	protected boolean writeFile(QualifiedName name, ExtraLanguageAppendable appendable, IExtraLanguageGeneratorContext context) {
		final ExtraLanguageAppendable fileAppendable = createAppendable(context);
		generateFileHeader(name, fileAppendable, context);

		final ImportManager importManager = appendable.getImportManager();
		if (importManager != null && !importManager.getImports().isEmpty()) {
			for (final String imported : importManager.getImports()) {
				final QualifiedName qn = getQualifiedNameConverter().toQualifiedName(imported);
				generateImportStatement(qn, fileAppendable, context);
			}
			fileAppendable.newLine();
		}

		fileAppendable.append(appendable.getContent());
		final String content = fileAppendable.getContent();

		if (!Strings.isEmpty(content)) {
			final String fileName = toFilename(name, FILENAME_SEPARATOR);
			final String outputConfiguration = getOutputConfigurationName();
			if (Strings.isEmpty(outputConfiguration)) {
				context.getFileSystemAccess().generateFile(fileName, content);
			} else {
				context.getFileSystemAccess().generateFile(fileName, outputConfiguration, content);
			}
			return true;
		}
		return false;
	}

	@Override
	public void beforeGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final IExtraLanguageGeneratorContext generatorContext = createGeneratorContext(fsa, context, input);
		final EList<EObject> contents = input.getContents();
		for (final EObject obj : contents) {
			if (canGenerateFor(obj)) {
				before(obj, generatorContext);
			}
		}
	}

	@Override
	public void doGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final IExtraLanguageGeneratorContext generatorContext = createGeneratorContext(fsa, context, input);
		final EList<EObject> contents = input.getContents();
		for (final EObject obj : contents) {
			if (canGenerateFor(obj)) {
				generate(obj, generatorContext);
			}
		}
	}

	/** Generate the given object.
	 *
	 * <p>This function calls the {@link #before(EObject, IExtraLanguageGeneratorContext)},
	 * {@link #generate(EObject, ExtraLanguageAppendable, IExtraLanguageGeneratorContext)}, and
	 * {@link #after(EObject, IExtraLanguageGeneratorContext)} functions.
	 *
	 * @param object the object.
	 * @param appendable the target for the generated content.
	 * @param context the context.
	 */
	@Override
	public void doGenerate(EObject object, ExtraLanguageAppendable appendable, IExtraLanguageGeneratorContext context) {
		try {
			before(object, context);
			generate(object, appendable, context);
		} finally {
			after(object, context);
		}
	}

	@Override
	public void afterGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final IExtraLanguageGeneratorContext generatorContext = createGeneratorContext(fsa, context, input);
		final EList<EObject> contents = input.getContents();
		for (final EObject obj : contents) {
			if (canGenerateFor(obj)) {
				after(obj, generatorContext);
			}
		}
	}

	/** Create the generator context for this generator.
	 *
	 * @param fsa the file system access.
	 * @param context the global context.
	 * @param resource the resource.
	 * @return the context.
	 */
	protected IExtraLanguageGeneratorContext createGeneratorContext(IFileSystemAccess2 fsa, IGeneratorContext context,
			Resource resource) {
		if (context instanceof IExtraLanguageGeneratorContext) {
			return (IExtraLanguageGeneratorContext) context;
		}
		return new ExtraLanguageGeneratorContext(context, fsa, this, resource);
	}

	/** Replies the identifier of the plugin which defines the generator.
	 *
	 * @return the plugin identifier.
	 */
	public abstract String getPluginID();

	/** Create the appendable object.
	 *
	 * @param context the generation context.
	 * @return the appendable object.
	 */
	protected abstract ExtraLanguageAppendable createAppendable(IExtraLanguageGeneratorContext context);

	/** Replies if this generator can generate resources from the given element.
	 *
	 * @param object the object for which the generation was queried.
	 * @return {@code true} for generating the resources, {@code false} for ignoring the object.
	 */
	@SuppressWarnings("static-method")
	protected boolean canGenerateFor(EObject object) {
		return !(object instanceof JvmIdentifiableElement);
	}

	/** Do something before the generation of the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 */
	protected void before(EObject object, IExtraLanguageGeneratorContext context) {
		this.beforeDispatcher.invoke(object, context);
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 */
	protected void generate(EObject object, IExtraLanguageGeneratorContext context) {
		this.generateDispatcher.invoke(object, context);
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param appendable the target for the generated content.
	 * @param context the context.
	 */
	protected void generate(EObject object, ExtraLanguageAppendable appendable, IExtraLanguageGeneratorContext context) {
		this.generateDispatcher2.invoke(object, appendable, context);
	}

	/** Generate the given object.
	 *
	 * @param expression the expression to be generated.
	 * @param needReturn the type of the expression in the context of a returned expression from a function.
	 * @param appendable the target for the generated content.
	 * @param context the context.
	 */
	protected void generate(XExpression expression, LightweightTypeReference needReturn, ExtraLanguageAppendable appendable,
			IExtraLanguageGeneratorContext context) {
		final IExpressionGenerator generator = getExpressionGenerator();
		if (generator == null) {
			throw new UnsupportedOperationException();
		}
		generator.generate(expression, needReturn, appendable, context);
	}

	/** Do something after the generation of the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 */
	protected void after(EObject object, IExtraLanguageGeneratorContext context) {
		this.afterDispatcher.invoke(object, context);
	}

	/** Generate the given script.
	 *
	 * @param script the script.
	 * @param context the context.
	 */
	protected void _generate(SarlScript script, IExtraLanguageGeneratorContext context) {
		if (script != null) {
			for (final XtendTypeDeclaration content : script.getXtendTypes()) {
				if (context.getCancelIndicator().isCanceled()) {
					return;
				}
				try {
					generate(content, context);
				} finally {
					context.clearData();
				}
			}
		}
	}

	/** Generate the import for the given name.
	 *
	 * @param importedQualifiedName the imported name.
	 * @param appendable the appendable.
	 * @param context the context.
	 */
	protected void generateImportStatement(QualifiedName importedQualifiedName, ExtraLanguageAppendable appendable,
			IExtraLanguageGeneratorContext context) {
		//
	}

	/** Generate the header of the file..
	 *
	 * @param qualifiedName the name of the type for which the file was created.
	 * @param appendable the appendable.
	 * @param context the context.
	 */
	protected void generateFileHeader(QualifiedName qualifiedName, ExtraLanguageAppendable appendable,
			IExtraLanguageGeneratorContext context) {
		//
	}

	/** Generate the members (except constructors) for a Python class.
	 *
	 * @param members the members to be added.
	 * @param it the output.
	 * @param context the generation context.
	 * @return {@code true} if a member was generated. {@code false} if no member was generated.
	 */
	protected boolean generateJvmMembers(List<? extends JvmMember> members, ExtraLanguageAppendable it,
			IExtraLanguageGeneratorContext context) {
		for (final JvmMember member : members) {
			if (!(member instanceof JvmConstructor)) {
				if (context.getCancelIndicator().isCanceled()) {
					return false;
				}
				generate(member, it, context);
			}
		}
		return true;
	}

	/** Generate the members (except constructors) for a Python class.
	 *
	 * @param members the members to be added.
	 * @param it the output.
	 * @param context the generation context.
	 * @return {@code true} if a member was generated. {@code false} if no member was generated.
	 */
	protected boolean generateSarlMembers(List<? extends XtendMember> members, ExtraLanguageAppendable it,
			IExtraLanguageGeneratorContext context) {
		for (final XtendMember member : members) {
			if (!(member instanceof SarlConstructor)) {
				if (context.getCancelIndicator().isCanceled()) {
					return false;
				}
				generate(member, it, context);
			}
		}
		return true;
	}

	/** Replies the merged list with the extended and implemented types.
	 *
	 * @param extension the extended type.
	 * @param implemented the implemented types.
	 * @return the super types.
	 */
	protected static List<JvmTypeReference> getSuperTypes(JvmTypeReference extension, List<? extends JvmTypeReference> implemented) {
		final List<JvmTypeReference> list = new ArrayList<>();
		if (extension != null) {
			list.add(extension);
		}
		if (implemented != null) {
			list.addAll(implemented);
		}
		return list;
	}

	/** Compute the expected type of the given expression.
	 *
	 * @param expr the expression.
	 * @return the expected type of the argument.
	 */
	protected LightweightTypeReference getExpectedType(XExpression expr) {
		final IResolvedTypes resolvedTypes = getTypeResolver().resolveTypes(expr);
		final LightweightTypeReference actualType = resolvedTypes.getActualType(expr);
		return actualType;
	}

}


