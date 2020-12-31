/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.lang.extralanguage.compiler;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import javax.inject.Inject;

import com.google.common.collect.Lists;
import com.google.inject.Injector;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
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
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.util.Utils;

/** Abstract implementation for the generator from SARL to an extra language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings({"checkstyle:classfanoutcomplexity"})
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

	private CommonTypeComputationServices services;

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

	/** Load a property file from the resources.
	 * This function is able to get the resource from an OSGi bundles if it is specified,
	 * or from the application classpath.
	 *
	 * @param filename the name of the resource, without the starting slash character.
	 * @param bundledPlugin the plugin that is able to provide the bundle that contains the resources, or {@code null}
	 *     to use the application classpath.
	 * @param readerClass the class that has called this function. It is used for obtaining the resource if
	 *     the bundled plugin is not provided.
	 * @param statusBuilder the builder of a status that takes the exception as parameter and creates a status object.
	 *     This builder is used only if the {@code bundledPlugin} is not {@code null}.
	 * @return the loaded resources.
	 */
	public static List<Pair<String, String>> loadPropertyFile(String filename, Plugin bundledPlugin,
			Class<?> readerClass,
			Function1<IOException, IStatus> statusBuilder) {
		final URL url;
		if (bundledPlugin != null) {
			url = FileLocator.find(
					bundledPlugin.getBundle(),
					Path.fromPortableString(filename),
					null);
		} else {
			url = readerClass.getClassLoader().getResource(filename);
		}
		if (url == null) {
			return Lists.newArrayList();
		}
		final OrderedProperties properties = new OrderedProperties();
		try (InputStream is = url.openStream()) {
			properties.load(is);
		} catch (IOException exception) {
			if (bundledPlugin != null) {
				bundledPlugin.getLog().log(statusBuilder.apply(exception));
			} else {
				throw new RuntimeException(exception);
			}
		}
		return properties.getOrderedProperties();
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

	/** Change the common type service.
	 *
	 * @param services the service.
	 */
	@Inject
	public void setCommonTypeComputationServices(CommonTypeComputationServices services) {
		this.services = services;
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
		final ExtraLanguageAppendable fileAppendable = createAppendable(null, context);
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

	/** Replies the identifier of the container of the generator's preferences.
	 *
	 * @return the identifier.
	 */
	protected abstract String getPreferenceID();

	@Override
	public void beforeGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final IExtraLanguageGeneratorContext generatorContext = createGeneratorContext(fsa, context, input);
		final EList<EObject> contents = input.getContents();
		for (final EObject obj : contents) {
			if (canGenerateFor(obj)) {
				before(obj, generatorContext);
				final Iterator<EObject> iterator = EcoreUtil.getAllContents(obj, false);
				while (iterator.hasNext()) {
					final EObject subobj = iterator.next();
					before(subobj, generatorContext);
				}
			}
		}
	}

	/** Initialize the given context. This function in invoked when the generator is called with a resource and
	 * before the generation on the resource's content is started.
	 *
	 * @param generatorContext the context to initialize.
	 * @since 0.8
	 */
	protected void initializeContext(IExtraLanguageGeneratorContext generatorContext) {
		//
	}

	@Override
	public void doGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final IExtraLanguageGeneratorContext generatorContext = createGeneratorContext(fsa, context, input);
		initializeContext(generatorContext);
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
				final Iterator<EObject> iterator = EcoreUtil.getAllContents(obj, false);
				while (iterator.hasNext()) {
					final EObject subobj = iterator.next();
					after(subobj, generatorContext);
				}
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
		return new ExtraLanguageGeneratorContext(context, fsa, this, resource, getPreferenceID());
	}

	/** Create the appendable object.
	 *
	 * @param thisType the current type for which the appendable should be created. If it is {@code null}, the import manager
	 *     of the appendable is not associated to a "this" type.
	 * @param context the generation context.
	 * @return the appendable object.
	 */
	protected abstract ExtraLanguageAppendable createAppendable(JvmDeclaredType thisType, IExtraLanguageGeneratorContext context);

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

	/** Replies the expected type of the given executable.
	 *
	 * @param executable the executable.
	 * @param declaredReturnType the declared return type, if one.
	 * @return the expected type or {@code null} if none ({@code void}).
	 */
	protected LightweightTypeReference getExpectedType(XtendExecutable executable, JvmTypeReference declaredReturnType) {
		if (declaredReturnType == null) {
			// Try to get any inferred return type.
			if (executable instanceof XtendFunction) {
				final XtendFunction function = (XtendFunction) executable;
				final JvmOperation operation = this.sarlAssociations.getDirectlyInferredOperation(function);
				if (operation != null) {
					return Utils.toLightweightTypeReference(operation.getReturnType(), this.services);
				}
			}
			if (!getEarlyExitComputer().isEarlyExit(executable.getExpression())) {
				return getExpectedType(executable.getExpression());
			}
			return null;
		}
		if (!"void".equals(declaredReturnType.getIdentifier())) { //$NON-NLS-1$
			return Utils.toLightweightTypeReference(declaredReturnType, this.services);
		}
		return null;
	}

	/** Specific properties with reading order.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	private static class OrderedProperties extends Properties {

		private static final long serialVersionUID = 162949168401947298L;

		private final List<Pair<String, String>> orderedElements = new ArrayList<>();

		OrderedProperties() {
			//
		}

		@Override
		public synchronized Object put(Object key, Object value) {
			this.orderedElements.add(new Pair<>(Objects.toString(key), Objects.toString(value)));
			return super.put(key, value);
		}

		/** Replies the ordered elements.
		 *
		 * @return the ordered elements.
		 */
		public List<Pair<String, String>> getOrderedProperties() {
			return this.orderedElements;
		}

	}

}


