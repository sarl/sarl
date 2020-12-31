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

package io.sarl.lang.mwe2.codebuilder.fragments;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Map;
import javax.inject.Inject;
import javax.inject.Singleton;

import com.google.inject.Binding;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.google.inject.name.Named;
import com.google.inject.name.Names;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.common.types.xtext.AbstractTypeScopeProvider;
import org.eclipse.xtext.common.types.xtext.ClasspathBasedTypeScopeProvider;
import org.eclipse.xtext.common.types.xtext.ui.JdtBasedSimpleTypeScopeProvider;
import org.eclipse.xtext.formatting.impl.AbstractTokenStream;
import org.eclipse.xtext.resource.SaveOptions;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.serializer.impl.Serializer;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.scoping.batch.DelegatingScopes;
import org.eclipse.xtext.xbase.scoping.batch.TypeScopes;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/** Generator of the abstract code builder.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AbstractAppenderBuilderFragment extends AbstractSubCodeBuilderFragment {

	private static final String SCOPE_PROVIDER_NAME =
			"io.sarl.lang.codebuilder.appenders.SourceAppender.providerType"; //$NON-NLS-1$

	@Override
	public void generate() {
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			generateAbstractAppender();
		}
	}

	private static void bind(BindingFactory factory, Class<?> type) {
		factory.addConfiguredBinding(
				"configureAbstractTypeScopeProviderForSourceAppender", new StringConcatenationClient() { //$NON-NLS-1$
					@Override
					protected void appendTo(TargetStringConcatenation it) {
				        it.append("binder.bind("); //$NON-NLS-1$
				        it.append(AbstractTypeScopeProvider.class);
				        it.append(".class).annotatedWith("); //$NON-NLS-1$
				        it.append(Names.class, ""); //$NON-NLS-1$
				        it.append(".named(\""); //$NON-NLS-1$
				        it.append(SCOPE_PROVIDER_NAME);
				        it.append("\")).to("); //$NON-NLS-1$
				        it.append(type);
				        it.append(".class);"); //$NON-NLS-1$
					}
				});
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		bind(factory, ClasspathBasedTypeScopeProvider.class);
	}

	@Override
	public void generateEclipseBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		bind(factory, JdtBasedSimpleTypeScopeProvider.class);
	}

	@Override
	public void generateIdeaBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		bind(factory, JdtBasedSimpleTypeScopeProvider.class);
	}

	/** Generate the abstract appender.
	 */
	@SuppressWarnings("checkstyle:all")
	protected void generateAbstractAppender() {
		final TypeReference abstractAppender = getCodeElementExtractor().getAbstractAppenderImpl();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Abstract implementation of an appender for the " //$NON-NLS-1$
						+ getLanguageName() + " language."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public abstract class "); //$NON-NLS-1$
				it.append(abstractAppender.getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic static final String OVERRIDEN_TYPE_SCOPE_PROVIDER_NAME = \""); //$NON-NLS-1$
				it.append(SCOPE_PROVIDER_NAME);
				it.append("\";"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(Injector.class);
				it.append(" originalInjector;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Named.class);
				it.append("(OVERRIDEN_TYPE_SCOPE_PROVIDER_NAME)"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(AbstractTypeScopeProvider.class);
				it.append(" scopeProvider;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(TypeScopes.class);
				it.append(" typeScopes;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate boolean isFormatting;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Set if this building is formatting the generated code."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param formatting <code>true</code> if the appender is formatting the generated code."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic void setFormatting(boolean formatting) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.isFormatting = formatting;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies if this building is formatting the generated code."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return <code>true</code> if the appender is formatting the generated code."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic boolean isFormatting() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.isFormatting;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the context for type resolution."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the context, or {@code null} if the Ecore object is the context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprotected abstract "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" getTypeResolutionContext();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Build the source code and put it into the given appender."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param appender the object that permits to create the source code."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic abstract void build("); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(";"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Build the source code and put it into the given appender."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param the object to serialize"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param appender the object that permits to create the source code."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the context for type resolution."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprotected void build("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" object, "); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" provider = getTypeResolutionContext();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (provider != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal "); //$NON-NLS-1$
				it.append(Map.class);
				it.append("<"); //$NON-NLS-1$
				it.append(Key.class);
				it.append("<?>, "); //$NON-NLS-1$
				it.append(Binding.class);
				it.append("<?>> bindings = this.originalInjector.getBindings();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(Injector.class);
				it.append(" localInjector = "); //$NON-NLS-1$
				it.append(getBuilderFactoryImpl());
				it.append(".createOverridingInjector(this.originalInjector, "); //$NON-NLS-1$
				it.append("(binder) -> binder.bind("); //$NON-NLS-1$
				it.append(AbstractTypeScopeProvider.class);
				it.append(".class).toInstance("); //$NON-NLS-1$
				it.append(abstractAppender.getSimpleName());
				it.append(".this.scopeProvider));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal "); //$NON-NLS-1$
				it.append(IScopeProvider.class);
				it.append(" oldDelegate = this.typeScopes.getDelegate();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tlocalInjector.injectMembers(this.typeScopes);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\ttry {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tfinal AppenderSerializer serializer = localInjector.getProvider(AppenderSerializer.class).get();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tserializer.serialize(object, appender, isFormatting());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} finally {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\ttry {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tfinal "); //$NON-NLS-1$
				it.append(Field.class);
				it.append(" f = "); //$NON-NLS-1$
				it.append(DelegatingScopes.class);
				it.append(".class.getDeclaredField(\"delegate\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tif (!f.isAccessible()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tf.setAccessible(true);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tf.set(this.typeScopes, oldDelegate);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t} catch ("); //$NON-NLS-1$
				it.append(Exception.class);
				it.append(" exception) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tthrow new "); //$NON-NLS-1$
				it.append(Error.class);
				it.append("(exception);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal AppenderSerializer serializer = this.originalInjector.getProvider(AppenderSerializer.class).get();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tserializer.serialize(object, appender, isFormatting());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Singleton.class);
				it.newLine();
				it.append("\tpublic static class AppenderSerializer extends "); //$NON-NLS-1$
				it.append(Serializer.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void serialize("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" object, "); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender, boolean isFormatting) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal AppenderBasedTokenStream stream = new AppenderBasedTokenStream(appender);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal "); //$NON-NLS-1$
				it.append(SaveOptions.class);
				it.append(" options;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (isFormatting) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\toptions = "); //$NON-NLS-1$
				it.append(SaveOptions.class);
				it.append(".newBuilder().format().getOptions();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\toptions = "); //$NON-NLS-1$
				it.append(SaveOptions.class);
				it.append(".defaultOptions();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tserialize(object, stream, options);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tstream.flush();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate static class AppenderBasedTokenStream extends "); //$NON-NLS-1$
				it.append(AbstractTokenStream.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tprivate final "); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic AppenderBasedTokenStream("); //$NON-NLS-1$
				it.append(ISourceAppender.class);
				it.append(" appender) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthis.appender = appender;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic String toString() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn this.appender.toString();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void writeHidden("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" grammarElement, String value) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(value)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.appender.append(value);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t\tpublic void writeSemantic("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" grammarElement, String value) throws "); //$NON-NLS-1$
				it.append(IOException.class);
				it.append(" {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!"); //$NON-NLS-1$
				it.append(Strings.class);
				it.append(".isEmpty(value)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tthis.appender.append(value);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the type reference for the given name in the given context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic abstract "); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(" newTypeRef(String typeName);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the type reference for the given name in the given context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic abstract "); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(" newTypeRef("); //$NON-NLS-1$
				it.append(Notifier.class);
				it.append(" context, String typeName);"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(abstractAppender, content);
		javaFile.writeTo(getSrcGen());
	}

}
