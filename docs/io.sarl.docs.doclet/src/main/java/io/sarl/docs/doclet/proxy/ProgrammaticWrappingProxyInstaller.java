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

package io.sarl.docs.doclet.proxy;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import javax.tools.FileObject;
import javax.tools.JavaFileManager;

import com.sun.javadoc.AnnotatedType;
import com.sun.javadoc.AnnotationDesc;
import com.sun.javadoc.AnnotationTypeDoc;
import com.sun.javadoc.AnnotationTypeElementDoc;
import com.sun.javadoc.AnnotationValue;
import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.ConstructorDoc;
import com.sun.javadoc.Doc;
import com.sun.javadoc.FieldDoc;
import com.sun.javadoc.MethodDoc;
import com.sun.javadoc.PackageDoc;
import com.sun.javadoc.ParamTag;
import com.sun.javadoc.Parameter;
import com.sun.javadoc.ParameterizedType;
import com.sun.javadoc.RootDoc;
import com.sun.javadoc.SeeTag;
import com.sun.javadoc.SerialFieldTag;
import com.sun.javadoc.SourcePosition;
import com.sun.javadoc.Tag;
import com.sun.javadoc.ThrowsTag;
import com.sun.javadoc.Type;
import com.sun.javadoc.TypeVariable;
import com.sun.javadoc.WildcardType;
import com.sun.source.util.TreePath;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symbol.ClassSymbol;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javac.code.Symbol.PackageSymbol;
import com.sun.tools.javac.code.Symbol.VarSymbol;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javadoc.AnnotationTypeDocImpl;
import com.sun.tools.javadoc.AnnotationTypeElementDocImpl;
import com.sun.tools.javadoc.ClassDocImpl;
import com.sun.tools.javadoc.ConstructorDocImpl;
import com.sun.tools.javadoc.DocEnv;
import com.sun.tools.javadoc.DocImpl;
import com.sun.tools.javadoc.ExecutableMemberDocImpl;
import com.sun.tools.javadoc.FieldDocImpl;
import com.sun.tools.javadoc.MethodDocImpl;
import com.sun.tools.javadoc.PackageDocImpl;
import com.sun.tools.javadoc.ProgramElementDocImpl;
import com.sun.tools.javadoc.RootDocImpl;

import io.sarl.docs.doclet.SarlConfiguration;
import io.sarl.docs.doclet.utils.Reflect;
import io.sarl.docs.doclet.utils.Utils;

/** Install the proxies for the {@code Doc} with the programmatic wrappers.
 *
 * <p>These proxies are implemented because the code of Javadoc contains hard casts to the implementation types.
 * {@link JreProxyInstaller In-memory proxies} cannot be use.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class ProgrammaticWrappingProxyInstaller implements ProxyInstaller {

	private final SarlConfiguration configuration;

	/** Constructor.
	 *
	 * @param configuration the configuration.
	 */
	public ProgrammaticWrappingProxyInstaller(SarlConfiguration configuration) {
		this.configuration = configuration;
	}

	@Override
	public RootDoc installProxies(RootDoc obj) {
		if (obj instanceof RootDocImpl) {
			return new RootDocWrapper((RootDocImpl) obj);
		}
		return obj;
	}

	@Override
	public void installProxies(SarlConfiguration configuration) {
		final RootDoc doc = configuration.root;
		if (!(doc instanceof Proxy) && (doc instanceof RootDocImpl)) {
			configuration.root = new RootDocWrapper((RootDocImpl) doc);
		}
	}

	@Override
	public void uninstallProxies(SarlConfiguration configuration) {
		configuration.root = unwrap(configuration.root);
	}

	private boolean isIncluded(Doc element) {
		return !this.configuration.getApidocExcluder().isExcluded(element);
	}

	/** Wrap a package doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public PackageDoc wrap(PackageDoc source) {
		if (source == null || source instanceof Proxy<?> || !(source instanceof PackageDocImpl)) {
			return source;
		}
		return new PackageDocWrapper((PackageDocImpl) source);
	}

	/** Wrap a package doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public PackageDoc[] wrap(PackageDoc[] source) {
		if (source == null) {
			return null;
		}
		final List<PackageDoc> list = new ArrayList<>();
		for (final PackageDoc element : source) {
			if (isIncluded(element)) {
				list.add(wrap(element));
			}
		}
		return Utils.toArray(source, list);
	}

	/** Wrap a class doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public ClassDoc wrap(ClassDoc source) {
		if (source == null || source instanceof Proxy<?> || !(source instanceof ClassDocImpl)) {
			return source;
		}
		return new ClassDocWrapper((ClassDocImpl) source);
	}

	/** Wrap a class doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public ClassDoc[] wrap(ClassDoc[] source) {
		if (source == null) {
			return null;
		}
		final List<ClassDoc> list = new ArrayList<>();
		for (final ClassDoc element : source) {
			if (isIncluded(element)) {
				list.add(wrap(element));
			}
		}
		return Utils.toArray(source, list);
	}

	/** Wrap a class doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public ClassDocImpl wrap(ClassDocImpl source) {
		if (source == null || source instanceof Proxy<?>) {
			return source;
		}
		return new ClassDocWrapper(source);
	}

	@Override
	public AnnotationTypeDoc wrap(AnnotationTypeDoc source) {
		if (source == null || source instanceof Proxy<?> || !(source instanceof AnnotationTypeDocImpl)) {
			return source;
		}
		return new AnnotationTypeDocWrapper((AnnotationTypeDocImpl) source);
	}

	/** Wrap a annotation type doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public AnnotationTypeDoc[] wrap(AnnotationTypeDoc[] source) {
		if (source == null) {
			return null;
		}
		final List<AnnotationTypeDoc> list = new ArrayList<>();
		for (final AnnotationTypeDoc element : source) {
			if (isIncluded(element)) {
				list.add(wrap(element));
			}
		}
		return Utils.toArray(source, list);
	}

	/** Wrap a field doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public FieldDoc wrap(FieldDoc source) {
		if (source == null || source instanceof Proxy<?> || !(source instanceof FieldDocImpl)) {
			return source;
		}
		return new FieldDocWrapper((FieldDocImpl) source);
	}

	/** Wrap a field doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public FieldDoc[] wrap(FieldDoc[] source) {
		if (source == null) {
			return null;
		}
		final List<FieldDoc> list = new ArrayList<>();
		for (final FieldDoc element : source) {
			if (isIncluded(element)) {
				list.add(wrap(element));
			}
		}
		return Utils.toArray(source, list);
	}

	/** Wrap a constructor doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public ConstructorDoc wrap(ConstructorDoc source) {
		if (source == null || source instanceof Proxy<?> || !(source instanceof ConstructorDocImpl)) {
			return source;
		}
		return new ConstructorDocWrapper((ConstructorDocImpl) source);
	}

	/** Wrap a constructor doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public ConstructorDoc[] wrap(ConstructorDoc[] source) {
		if (source == null) {
			return null;
		}
		final List<ConstructorDoc> list = new ArrayList<>();
		for (final ConstructorDoc element : source) {
			if (isIncluded(element)) {
				list.add(wrap(element));
			}
		}
		return Utils.toArray(source, list);
	}

	/** Wrap a method doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public MethodDoc wrap(MethodDoc source) {
		if (source == null || source instanceof Proxy<?> || !(source instanceof MethodDocImpl)) {
			return source;
		}
		return new MethodDocWrapper((MethodDocImpl) source);
	}

	/** Wrap a method doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public MethodDocImpl wrap(MethodDocImpl source) {
		if (source == null || source instanceof Proxy<?>) {
			return source;
		}
		return new MethodDocWrapper(source);
	}

	/** Wrap a method doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public MethodDoc[] wrap(MethodDoc[] source) {
		if (source == null) {
			return null;
		}
		final List<MethodDoc> list = new ArrayList<>();
		for (final MethodDoc element : source) {
			if (isIncluded(element)) {
				list.add(wrap(element));
			}
		}
		return Utils.toArray(source, list);
	}

	/** Wrap a annotation type element doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public AnnotationTypeElementDoc wrap(AnnotationTypeElementDoc source) {
		if (source == null || source instanceof Proxy<?> || !(source instanceof AnnotationTypeElementDocImpl)) {
			return source;
		}
		return new AnnotationTypeElementDocWrapper((AnnotationTypeElementDocImpl) source);
	}

	/** Wrap a annotation type element doc.
	 *
	 * @param source the source
	 * @return the wrapper.
	 */
	public AnnotationTypeElementDoc[] wrap(AnnotationTypeElementDoc[] source) {
		if (source == null) {
			return null;
		}
		final List<AnnotationTypeElementDoc> list = new ArrayList<>();
		for (final AnnotationTypeElementDoc element : source) {
			if (isIncluded(element)) {
				list.add(wrap(element));
			}
		}
		return Utils.toArray(source, list);
	}

	private static DocEnv getEnv(DocImpl obj) {
		return Reflect.getField(obj, "env", DocImpl.class, DocEnv.class); //$NON-NLS-1$
	}

	private static Symbol getSym(ProgramElementDocImpl obj) {
		return Reflect.getField(obj, "sym", ProgramElementDocImpl.class, Symbol.class); //$NON-NLS-1$
	}

	private static PackageSymbol getSym(PackageDocImpl obj) {
		return Reflect.getField(obj, "sym", PackageDocImpl.class, PackageSymbol.class); //$NON-NLS-1$
	}

	@Override
	@SuppressWarnings({ "unchecked" })
	public <T> T unwrap(T source) {
		if (source instanceof Proxy) {
			return (T) ((Proxy<?>) source).getDelegate();
		}
		return source;
	}

	/** Proxy definition.
	 *
	 * @param <T> the type of the delegate.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	private interface Proxy<T extends Doc> {

		/** Replies the delegate.
		 *
		 * @return the delegate.
		 */
		T getDelegate();

	}

	/** Wrapper for RootDoc.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	@SuppressWarnings("checkstyle:all")
	private class RootDocWrapper extends RootDocImpl implements Proxy<RootDocImpl> {

		private final RootDocImpl delegate;

		@SuppressWarnings("synthetic-access")
		RootDocWrapper(RootDocImpl delegate) {
			super(getEnv(delegate), Utils.emptyList(), Utils.emptyList());
			Reflect.copyFields(RootDocImpl.class, this, delegate);
			this.delegate = delegate;
		}

		@Override
		public RootDocImpl getDelegate() {
			return this.delegate;
		}

		@Override
		public boolean equals(Object obj) {
			return this.delegate.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.delegate.hashCode();
		}

		@Override
		public Tag[] firstSentenceTags() {
			return this.delegate.firstSentenceTags();
		}

		@Override
		public String getRawCommentText() {
			return this.delegate.getRawCommentText();
		}

		@Override
		public Tag[] inlineTags() {
			return this.delegate.inlineTags();
		}

		@Override
		public boolean isAnnotationType() {
			return this.delegate.isAnnotationType();
		}

		@Override
		public boolean isAnnotationTypeElement() {
			return this.delegate.isAnnotationTypeElement();
		}

		@Override
		public boolean isClass() {
			return this.delegate.isClass();
		}

		@Override
		public boolean isConstructor() {
			return this.delegate.isConstructor();
		}

		@Override
		public boolean isEnum() {
			return this.delegate.isEnum();
		}

		@Override
		public boolean isEnumConstant() {
			return this.delegate.isEnumConstant();
		}

		@Override
		public boolean isError() {
			return this.delegate.isError();
		}

		@Override
		public boolean isException() {
			return this.delegate.isException();
		}

		@Override
		public boolean isField() {
			return this.delegate.isField();
		}

		@Override
		public boolean isIncluded() {
			return this.delegate.isIncluded();
		}

		@Override
		public boolean isInterface() {
			return isInterface();
		}

		@Override
		public boolean isMethod() {
			return this.delegate.isMethod();
		}

		@Override
		public boolean isOrdinaryClass() {
			return this.delegate.isOrdinaryClass();
		}

		@Override
		public String name() {
			return this.delegate.name();
		}

		@Override
		public SourcePosition position() {
			return this.delegate.position();
		}

		@Override
		public SeeTag[] seeTags() {
			return this.delegate.seeTags();
		}

		@Override
		public void setRawCommentText(String arg0) {
			this.delegate.setRawCommentText(arg0);
			Reflect.copyFields(RootDocImpl.class, this, this.delegate);
		}

		@Override
		public Tag[] tags() {
			return this.delegate.tags();
		}

		@Override
		public Tag[] tags(String arg0) {
			return this.delegate.tags(arg0);
		}

		@Override
		public void printError(String arg0) {
			this.delegate.printError(arg0);
		}

		@Override
		public void printError(SourcePosition arg0, String arg1) {
			this.delegate.printError(arg0, arg1);
		}

		@Override
		public void printNotice(String arg0) {
			this.delegate.printNotice(arg0);
		}

		@Override
		public void printNotice(SourcePosition arg0, String arg1) {
			this.delegate.printNotice(arg0, arg1);
		}

		@Override
		public void printWarning(String arg0) {
			this.delegate.printWarning(arg0);
		}

		@Override
		public void printWarning(SourcePosition arg0, String arg1) {
			this.delegate.printWarning(arg0, arg1);
		}

		@Override
		public ClassDoc classNamed(String arg0) {
			return wrap(this.delegate.classNamed(arg0));
		}

		@Override
		public ClassDoc[] classes() {
			return wrap(this.delegate.classes());
		}

		@Override
		public String[][] options() {
			return this.delegate.options();
		}

		@Override
		public PackageDoc packageNamed(String name) {
			return wrap(this.delegate.packageNamed(name));
		}

		@Override
		public ClassDoc[] specifiedClasses() {
			return wrap(this.delegate.specifiedClasses());
		}

		@Override
		public PackageDoc[] specifiedPackages() {
			return wrap(this.delegate.specifiedPackages());
		}

		@Override
		public String qualifiedName() {
			return this.delegate.qualifiedName();
		}

		@Override
		protected String documentation() {
			return Reflect.callFunc(this.delegate, DocImpl.class, String.class, "documentation", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		public Locale getLocale() {
			return this.delegate.getLocale();
		}

		@Override
		public JavaFileManager getFileManager() {
			return this.delegate.getFileManager();
		}

		@Override
		public void initDocLint(Collection<String> opts, Collection<String> customTagNames) {
			this.delegate.initDocLint(opts, customTagNames);
		}

		@Override
		public boolean isFunctionalInterface(AnnotationDesc annotationDesc) {
			return this.delegate.isFunctionalInterface(annotationDesc);
		}

		@Override
		public boolean showTagMessages() {
			return this.delegate.showTagMessages();
		}

		@Override
		public String commentText() {
			return this.delegate.commentText();
		}

		@Override
		public String toString() {
			return this.delegate.toString();
		}

		@Override
		public int compareTo(Object obj) {
			return this.delegate.compareTo(obj);
		}
		
	}

	/** Wrapper for ClassDoc.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	@SuppressWarnings("checkstyle:all")
	private class ClassDocWrapper extends ClassDocImpl implements Proxy<ClassDocImpl> {

		private final ClassDocImpl delegate;

		@SuppressWarnings("synthetic-access")
		ClassDocWrapper(ClassDocImpl delegate) {
			super(getEnv(delegate), (ClassSymbol) getSym(delegate));
			Reflect.copyFields(ClassDocImpl.class, this, delegate);
			this.delegate = delegate;
		}

		@Override
		public ClassDocImpl getDelegate() {
			return this.delegate;
		}

		@Override
		public boolean equals(Object obj) {
			return this.delegate.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.delegate.hashCode();
		}

		@Override
		public AnnotationDesc[] annotations() {
			return this.delegate.annotations();
		}

		@Override
		public ClassDoc containingClass() {
			return wrap(this.delegate.containingClass());
		}

		@Override
		public PackageDoc containingPackage() {
			return wrap(this.delegate.containingPackage());
		}

		@Override
		public boolean isFinal() {
			return this.delegate.isFinal();
		}

		@Override
		public boolean isPackagePrivate() {
			return this.delegate.isPackagePrivate();
		}

		@Override
		public boolean isPrivate() {
			return this.delegate.isPrivate();
		}

		@Override
		public boolean isProtected() {
			return this.delegate.isProtected();
		}

		@Override
		public boolean isPublic() {
			return this.delegate.isPublic();
		}

		@Override
		public boolean isStatic() {
			return this.delegate.isStatic();
		}

		@Override
		public int modifierSpecifier() {
			return this.delegate.modifierSpecifier();
		}

		@Override
		public String modifiers() {
			return this.delegate.modifiers();
		}

		@Override
		public String qualifiedName() {
			return this.delegate.qualifiedName();
		}

		@Override
		public String commentText() {
			return this.delegate.commentText();
		}

		@Override
		public int compareTo(Object arg0) {
			return this.delegate.compareTo(unwrap(arg0));
		}

		@Override
		public Tag[] firstSentenceTags() {
			return this.delegate.firstSentenceTags();
		}

		@Override
		public String getRawCommentText() {
			return this.delegate.getRawCommentText();
		}

		@Override
		public Tag[] inlineTags() {
			return this.delegate.inlineTags();
		}

		@Override
		public boolean isAnnotationType() {
			return this.delegate.isAnnotationType();
		}

		@Override
		public boolean isAnnotationTypeElement() {
			return this.delegate.isAnnotationTypeElement();
		}

		@Override
		public boolean isClass() {
			return this.delegate.isClass();
		}

		@Override
		public boolean isConstructor() {
			return this.delegate.isConstructor();
		}

		@Override
		public boolean isEnum() {
			return this.delegate.isEnum();
		}

		@Override
		public boolean isEnumConstant() {
			return this.delegate.isEnumConstant();
		}

		@Override
		public boolean isError() {
			return this.delegate.isError();
		}

		@Override
		public boolean isException() {
			return this.delegate.isException();
		}

		@Override
		public boolean isField() {
			return this.delegate.isField();
		}

		@Override
		public boolean isIncluded() {
			return this.delegate.isIncluded();
		}

		@Override
		public boolean isInterface() {
			return this.delegate.isInterface();
		}

		@Override
		public boolean isMethod() {
			return this.delegate.isMethod();
		}

		@Override
		public boolean isOrdinaryClass() {
			return this.delegate.isOrdinaryClass();
		}

		@Override
		public String name() {
			return this.delegate.name();
		}

		@Override
		public SourcePosition position() {
			return this.delegate.position();
		}

		@Override
		public SeeTag[] seeTags() {
			return this.delegate.seeTags();
		}

		@Override
		public void setRawCommentText(String arg0) {
			this.delegate.setRawCommentText(arg0);
			Reflect.copyFields(ClassDocImpl.class, this, this.delegate);
		}

		@Override
		public Tag[] tags() {
			return this.delegate.tags();
		}

		@Override
		public Tag[] tags(String arg0) {
			return this.delegate.tags(arg0);
		}

		@Override
		public AnnotatedType asAnnotatedType() {
			return this.delegate.asAnnotatedType();
		}

		@Override
		public AnnotationTypeDoc asAnnotationTypeDoc() {
			return wrap(this.delegate.asAnnotationTypeDoc());
		}

		@Override
		public ClassDoc asClassDoc() {
			return wrap(this.delegate.asClassDoc());
		}

		@Override
		public ParameterizedType asParameterizedType() {
			return this.delegate.asParameterizedType();
		}

		@Override
		public TypeVariable asTypeVariable() {
			return this.delegate.asTypeVariable();
		}

		@Override
		public WildcardType asWildcardType() {
			return this.delegate.asWildcardType();
		}

		@Override
		public String dimension() {
			return this.delegate.dimension();
		}

		@Override
		public Type getElementType() {
			return this.delegate.getElementType();
		}

		@Override
		public boolean isPrimitive() {
			return this.delegate.isPrimitive();
		}

		@Override
		public String qualifiedTypeName() {
			return this.delegate.qualifiedTypeName();
		}

		@Override
		public String simpleTypeName() {
			return this.delegate.simpleTypeName();
		}

		@Override
		public String typeName() {
			return this.delegate.typeName();
		}

		@Override
		public ConstructorDoc[] constructors() {
			return wrap(this.delegate.constructors());
		}

		@Override
		public ConstructorDoc[] constructors(boolean arg0) {
			return wrap(this.delegate.constructors(arg0));
		}

		@Override
		public boolean definesSerializableFields() {
			return this.delegate.definesSerializableFields();
		}

		@Override
		public FieldDoc[] enumConstants() {
			return wrap(this.delegate.enumConstants());
		}

		@Override
		public FieldDoc[] fields() {
			return wrap(this.delegate.fields());
		}

		@Override
		public FieldDoc[] fields(boolean arg0) {
			return wrap(this.delegate.fields(arg0));
		}

		@Override
		public ClassDoc findClass(String name) {
			return wrap(this.delegate.findClass(name));
		}

		/**
		 * {@inheritDoc}.
		 * @deprecated no info
		 */
		@Override
		@Deprecated
		public ClassDoc[] importedClasses() {
			return wrap(this.delegate.importedClasses());
		}

		/**
		 * {@inheritDoc}.
		 * @deprecated no info
		 */
		@Override
		@Deprecated
		public PackageDoc[] importedPackages() {
			return wrap(this.delegate.importedPackages());
		}

		/** {@inheritDoc}
		 */
		@Override
		public ClassDoc[] innerClasses() {
			return wrap(this.delegate.innerClasses());
		}

		@Override
		public ClassDoc[] innerClasses(boolean arg0) {
			return wrap(this.delegate.innerClasses(arg0));
		}

		@Override
		public Type[] interfaceTypes() {
			return this.delegate.interfaceTypes();
		}

		@Override
		public ClassDoc[] interfaces() {
			return wrap(this.delegate.interfaces());
		}

		@Override
		public boolean isAbstract() {
			return this.delegate.isAbstract();
		}

		@Override
		public boolean isExternalizable() {
			return this.delegate.isExternalizable();
		}

		@Override
		public boolean isSerializable() {
			return this.delegate.isSerializable();
		}

		@Override
		public MethodDoc[] methods() {
			return wrap(this.delegate.methods());
		}

		@Override
		public MethodDoc[] methods(boolean arg0) {
			return wrap(this.delegate.methods(arg0));
		}

		@Override
		public FieldDoc[] serializableFields() {
			return wrap(this.delegate.serializableFields());
		}

		@Override
		public MethodDoc[] serializationMethods() {
			return wrap(this.delegate.serializationMethods());
		}

		@Override
		public boolean subclassOf(ClassDoc type) {
			return this.delegate.subclassOf(unwrap(type));
		}

		@Override
		public ClassDoc superclass() {
			return wrap(this.delegate.superclass());
		}

		@Override
		public Type superclassType() {
			return this.delegate.superclassType();
		}

		@Override
		public ParamTag[] typeParamTags() {
			return this.delegate.typeParamTags();
		}

		@Override
		public TypeVariable[] typeParameters() {
			return this.delegate.typeParameters();
		}

		@Override
		protected long getFlags() {
			return Reflect.callFunc(this.delegate, ClassDocImpl.class, long.class, "getFlags", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected ClassSymbol getContainingClass() {
			return Reflect.callFunc(this.delegate, ClassDocImpl.class, ClassSymbol.class, "getContainingClass", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		public boolean isThrowable() {
			return this.delegate.isThrowable();
		}

		@Override
		public boolean isSynthetic() {
			return this.delegate.isSynthetic();
		}

		@Override
		public String toString() {
			return this.delegate.toString();
		}

		@Override
		public MethodDocImpl findMethod(String methodName, String[] paramTypes) {
			return wrap(this.delegate.findMethod(methodName, paramTypes));
		}

		@Override
		public ConstructorDoc findConstructor(String constrName, String[] paramTypes) {
			return wrap(this.delegate.findConstructor(constrName, paramTypes));
		}

		@Override
		public FieldDoc findField(String fieldName) {
			return wrap(this.delegate.findField(fieldName));
		}

		@Override
		protected int getModifiers() {
			return Reflect.callFunc(this.delegate, ProgramElementDocImpl.class, int.class, "getModifiers", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected String documentation() {
			return Reflect.callFunc(this.delegate, DocImpl.class, String.class, "documentation", new Class<?>[0]); //$NON-NLS-1$
		}

	}

	/** Wrapper for PackageDoc.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	@SuppressWarnings("checkstyle:all")
	private class PackageDocWrapper extends PackageDocImpl implements Proxy<PackageDocImpl> {

		private final PackageDocImpl delegate;

		@SuppressWarnings("synthetic-access")
		PackageDocWrapper(PackageDocImpl delegate) {
			super(getEnv(delegate), getSym(delegate));
			Reflect.copyFields(PackageDocImpl.class, this, delegate);
			this.delegate = delegate;
		}

		@Override
		public PackageDocImpl getDelegate() {
			return this.delegate;
		}

		@Override
		public boolean equals(Object obj) {
			return this.delegate.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.delegate.hashCode();
		}

		@Override
		public String commentText() {
			return this.delegate.commentText();
		}

		@Override
		public int compareTo(Object obj) {
			return this.delegate.compareTo(unwrap(obj));
		}

		@Override
		public Tag[] firstSentenceTags() {
			return this.delegate.firstSentenceTags();
		}

		@Override
		public String getRawCommentText() {
			return this.delegate.getRawCommentText();
		}

		@Override
		public Tag[] inlineTags() {
			return this.delegate.inlineTags();
		}

		@Override
		public boolean isAnnotationType() {
			return this.delegate.isAnnotationType();
		}

		@Override
		public boolean isAnnotationTypeElement() {
			return this.delegate.isAnnotationTypeElement();
		}

		@Override
		public boolean isClass() {
			return this.delegate.isClass();
		}

		@Override
		public boolean isConstructor() {
			return this.delegate.isConstructor();
		}

		@Override
		public boolean isEnum() {
			return this.delegate.isEnum();
		}

		@Override
		public boolean isEnumConstant() {
			return this.delegate.isEnumConstant();
		}

		@Override
		public boolean isError() {
			return this.delegate.isError();
		}

		@Override
		public boolean isException() {
			return this.delegate.isException();
		}

		@Override
		public boolean isField() {
			return this.delegate.isField();
		}

		@Override
		public boolean isIncluded() {
			return this.delegate.isIncluded();
		}

		@Override
		public boolean isInterface() {
			return this.delegate.isInterface();
		}

		@Override
		public boolean isMethod() {
			return this.delegate.isMethod();
		}

		@Override
		public boolean isOrdinaryClass() {
			return this.delegate.isOrdinaryClass();
		}

		@Override
		public String name() {
			return this.delegate.name();
		}

		@Override
		public SourcePosition position() {
			return this.delegate.position();
		}

		@Override
		public SeeTag[] seeTags() {
			return this.delegate.seeTags();
		}

		@Override
		public void setRawCommentText(String text) {
			this.delegate.setRawCommentText(text);
			Reflect.copyFields(PackageDocImpl.class, this, this.delegate);
		}

		@Override
		public Tag[] tags() {
			return this.delegate.tags();
		}

		@Override
		public Tag[] tags(String arg0) {
			return this.delegate.tags(arg0);
		}

		@Override
		public ClassDoc[] allClasses() {
			return wrap(this.delegate.allClasses());
		}

		@Override
		public ClassDoc[] allClasses(boolean arg0) {
			return wrap(this.delegate.allClasses(arg0));
		}

		@Override
		public AnnotationTypeDoc[] annotationTypes() {
			return wrap(this.delegate.annotationTypes());
		}

		@Override
		public AnnotationDesc[] annotations() {
			return this.delegate.annotations();
		}

		@Override
		public ClassDoc[] enums() {
			return wrap(this.delegate.enums());
		}

		@Override
		public ClassDoc[] errors() {
			return wrap(this.delegate.errors());
		}

		@Override
		public ClassDoc[] exceptions() {
			return wrap(this.delegate.exceptions());
		}

		@Override
		public ClassDoc findClass(String name) {
			return wrap(this.delegate.findClass(name));
		}

		@Override
		public ClassDoc[] interfaces() {
			return wrap(this.delegate.interfaces());
		}

		@Override
		public ClassDoc[] ordinaryClasses() {
			return wrap(this.delegate.ordinaryClasses());
		}

		@Override
		public void setTreePath(TreePath treePath) {
			this.delegate.setTreePath(treePath);
			Reflect.copyFields(PackageDocImpl.class, this, this.delegate);
		}

		@Override
		protected String documentation() {
			return Reflect.callFunc(this.delegate, PackageDocImpl.class,
					String.class, "documentation", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		public void addAllClassesTo(ListBuffer<ClassDocImpl> list) {
			final ListBuffer<ClassDocImpl> buf = new ListBuffer<>();
			this.delegate.addAllClassesTo(buf);
			for (final ClassDocImpl doc : buf) {
				list.add(wrap(doc));
			}
		}

		@Override
		public String qualifiedName() {
			return this.delegate.qualifiedName();
		}

		@Override
		public void setDocPath(FileObject path) {
			this.delegate.setDocPath(path);
		}

	}

	/** Wrapper for AnnotationTypeDoc.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	@SuppressWarnings("checkstyle:all")
	private class AnnotationTypeDocWrapper extends AnnotationTypeDocImpl implements Proxy<AnnotationTypeDocImpl> {

		private final AnnotationTypeDocImpl delegate;

		@SuppressWarnings("synthetic-access")
		AnnotationTypeDocWrapper(AnnotationTypeDocImpl delegate) {
			super(getEnv(delegate), (ClassSymbol) getSym(delegate));
			Reflect.copyFields(AnnotationTypeDocImpl.class, this, delegate);
			this.delegate = delegate;
		}

		@Override
		public AnnotationTypeDocImpl getDelegate() {
			return this.delegate;
		}

		@Override
		public boolean equals(Object obj) {
			return this.delegate.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.delegate.hashCode();
		}

		@Override
		public ConstructorDoc[] constructors() {
			return wrap(this.delegate.constructors());
		}

		@Override
		public ConstructorDoc[] constructors(boolean arg0) {
			return wrap(this.delegate.constructors(arg0));
		}

		@Override
		public boolean definesSerializableFields() {
			return this.delegate.definesSerializableFields();
		}

		@Override
		public FieldDoc[] enumConstants() {
			return wrap(this.delegate.enumConstants());
		}

		@Override
		public FieldDoc[] fields() {
			return wrap(this.delegate.fields());
		}

		@Override
		public FieldDoc[] fields(boolean arg0) {
			return wrap(this.delegate.fields(arg0));
		}

		@Override
		public ClassDoc findClass(String name) {
			return wrap(this.delegate.findClass(name));
		}

		/**
		 * {@inheritDoc}.
		 * @deprecated no info.
		 */
		@Override
		@Deprecated
		public ClassDoc[] importedClasses() {
			return wrap(this.delegate.importedClasses());
		}

		/** {@inheritDoc}.
		 * @deprecated noinfo
		 */
		@Override
		@Deprecated
		public PackageDoc[] importedPackages() {
			return wrap(this.delegate.importedPackages());
		}

		@Override
		public ClassDoc[] innerClasses() {
			return wrap(this.delegate.innerClasses());
		}

		@Override
		public ClassDoc[] innerClasses(boolean arg0) {
			return wrap(this.delegate.innerClasses(arg0));
		}

		@Override
		public Type[] interfaceTypes() {
			return this.delegate.interfaceTypes();
		}

		@Override
		public ClassDoc[] interfaces() {
			return wrap(this.delegate.interfaces());
		}

		@Override
		public boolean isAbstract() {
			return this.delegate.isAbstract();
		}

		@Override
		public boolean isExternalizable() {
			return this.delegate.isExternalizable();
		}

		@Override
		public boolean isSerializable() {
			return this.delegate.isSerializable();
		}

		@Override
		public MethodDoc[] methods() {
			return wrap(this.delegate.methods());
		}

		@Override
		public MethodDoc[] methods(boolean arg0) {
			return wrap(this.delegate.methods(arg0));
		}

		@Override
		public FieldDoc[] serializableFields() {
			return wrap(this.delegate.serializableFields());
		}

		@Override
		public MethodDoc[] serializationMethods() {
			return wrap(this.delegate.serializationMethods());
		}

		@Override
		public boolean subclassOf(ClassDoc doc) {
			return this.delegate.subclassOf(unwrap(doc));
		}

		@Override
		public ClassDoc superclass() {
			return wrap(this.delegate.superclass());
		}

		@Override
		public Type superclassType() {
			return this.delegate.superclassType();
		}

		@Override
		public ParamTag[] typeParamTags() {
			return this.delegate.typeParamTags();
		}

		@Override
		public TypeVariable[] typeParameters() {
			return this.delegate.typeParameters();
		}

		@Override
		public AnnotationDesc[] annotations() {
			return this.delegate.annotations();
		}

		@Override
		public ClassDoc containingClass() {
			return wrap(this.delegate.containingClass());
		}

		@Override
		public PackageDoc containingPackage() {
			return wrap(this.delegate.containingPackage());
		}

		@Override
		public boolean isFinal() {
			return this.delegate.isFinal();
		}

		@Override
		public boolean isPackagePrivate() {
			return this.delegate.isPackagePrivate();
		}

		@Override
		public boolean isPrivate() {
			return this.delegate.isPrivate();
		}

		@Override
		public boolean isProtected() {
			return this.delegate.isProtected();
		}

		@Override
		public boolean isPublic() {
			return this.delegate.isPublic();
		}

		@Override
		public boolean isStatic() {
			return this.delegate.isStatic();
		}

		@Override
		public int modifierSpecifier() {
			return this.delegate.modifierSpecifier();
		}

		@Override
		public String modifiers() {
			return this.delegate.modifiers();
		}

		@Override
		public String qualifiedName() {
			return this.delegate.qualifiedName();
		}

		@Override
		public String commentText() {
			return this.delegate.commentText();
		}

		@Override
		public int compareTo(Object obj) {
			return this.delegate.compareTo(unwrap(obj));
		}

		@Override
		public Tag[] firstSentenceTags() {
			return this.delegate.firstSentenceTags();
		}

		@Override
		public String getRawCommentText() {
			return this.delegate.getRawCommentText();
		}

		@Override
		public Tag[] inlineTags() {
			return this.delegate.inlineTags();
		}

		@Override
		public boolean isAnnotationType() {
			return this.delegate.isAnnotationType();
		}

		@Override
		public boolean isAnnotationTypeElement() {
			return this.delegate.isAnnotationTypeElement();
		}

		@Override
		public boolean isClass() {
			return this.delegate.isClass();
		}

		@Override
		public boolean isConstructor() {
			return this.delegate.isConstructor();
		}

		@Override
		public boolean isEnum() {
			return this.delegate.isEnum();
		}

		@Override
		public boolean isEnumConstant() {
			return this.delegate.isEnumConstant();
		}

		@Override
		public boolean isError() {
			return this.delegate.isError();
		}

		@Override
		public boolean isException() {
			return this.delegate.isException();
		}

		@Override
		public boolean isField() {
			return this.delegate.isField();
		}

		@Override
		public boolean isIncluded() {
			return this.delegate.isIncluded();
		}

		@Override
		public boolean isInterface() {
			return this.delegate.isInterface();
		}

		@Override
		public boolean isMethod() {
			return this.delegate.isMethod();
		}

		@Override
		public boolean isOrdinaryClass() {
			return this.delegate.isOrdinaryClass();
		}

		@Override
		public String name() {
			return this.delegate.name();
		}

		@Override
		public SourcePosition position() {
			return this.delegate.position();
		}

		@Override
		public SeeTag[] seeTags() {
			return this.delegate.seeTags();
		}

		@Override
		public void setRawCommentText(String text) {
			this.delegate.setRawCommentText(text);
			Reflect.copyFields(AnnotationTypeDocImpl.class, this, this.delegate);
		}

		@Override
		public Tag[] tags() {
			return this.delegate.tags();
		}

		@Override
		public Tag[] tags(String arg0) {
			return this.delegate.tags(arg0);
		}

		@Override
		public AnnotatedType asAnnotatedType() {
			return this.delegate.asAnnotatedType();
		}

		@Override
		public AnnotationTypeDoc asAnnotationTypeDoc() {
			return wrap(this.delegate.asAnnotationTypeDoc());
		}

		@Override
		public ClassDoc asClassDoc() {
			return wrap(this.delegate.asClassDoc());
		}

		@Override
		public ParameterizedType asParameterizedType() {
			return this.delegate.asParameterizedType();
		}

		@Override
		public TypeVariable asTypeVariable() {
			return this.delegate.asTypeVariable();
		}

		@Override
		public WildcardType asWildcardType() {
			return this.delegate.asWildcardType();
		}

		@Override
		public String dimension() {
			return this.delegate.dimension();
		}

		@Override
		public Type getElementType() {
			return this.delegate.getElementType();
		}

		@Override
		public boolean isPrimitive() {
			return this.delegate.isPrimitive();
		}

		@Override
		public String qualifiedTypeName() {
			return this.delegate.qualifiedName();
		}

		@Override
		public String simpleTypeName() {
			return this.delegate.simpleTypeName();
		}

		@Override
		public String typeName() {
			return this.delegate.typeName();
		}

		@Override
		public AnnotationTypeElementDoc[] elements() {
			return wrap(this.delegate.elements());
		}

		@Override
		protected long getFlags() {
			return Reflect.callFunc(this.delegate, ClassDocImpl.class, long.class, "getFlags", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected ClassSymbol getContainingClass() {
			return Reflect.callFunc(this.delegate, ClassDocImpl.class, ClassSymbol.class, "getContainingClass", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		public boolean isThrowable() {
			return this.delegate.isThrowable();
		}

		@Override
		public boolean isSynthetic() {
			return this.delegate.isSynthetic();
		}

		@Override
		public String toString() {
			return this.delegate.toString();
		}

		@Override
		public MethodDocImpl findMethod(String methodName, String[] paramTypes) {
			return wrap(this.delegate.findMethod(methodName, paramTypes));
		}

		@Override
		public ConstructorDoc findConstructor(String constrName, String[] paramTypes) {
			return wrap(this.delegate.findConstructor(constrName, paramTypes));
		}

		@Override
		public FieldDoc findField(String fieldName) {
			return wrap(this.delegate.findField(fieldName));
		}

		@Override
		protected int getModifiers() {
			return Reflect.callFunc(this.delegate, ProgramElementDocImpl.class, int.class, "getModifiers", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected String documentation() {
			return Reflect.callFunc(this.delegate, DocImpl.class, String.class, "documentation", new Class<?>[0]); //$NON-NLS-1$
		}

		
	}

	/** Wrapper for FieldDoc.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	@SuppressWarnings("checkstyle:all")
	private class FieldDocWrapper extends FieldDocImpl implements Proxy<FieldDocImpl> {

		private final FieldDocImpl delegate;

		@SuppressWarnings("synthetic-access")
		FieldDocWrapper(FieldDocImpl delegate) {
			super(getEnv(delegate), (VarSymbol) getSym(delegate));
			Reflect.copyFields(FieldDocImpl.class, this, delegate);
			this.delegate = delegate;
		}

		@Override
		public FieldDocImpl getDelegate() {
			return this.delegate;
		}

		@Override
		public boolean equals(Object obj) {
			return this.delegate.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.delegate.hashCode();
		}

		@Override
		public boolean isSynthetic() {
			return this.delegate.isSynthetic();
		}

		@Override
		public AnnotationDesc[] annotations() {
			return this.delegate.annotations();
		}

		@Override
		public ClassDoc containingClass() {
			return wrap(this.delegate.containingClass());
		}

		@Override
		public PackageDoc containingPackage() {
			return wrap(this.delegate.containingPackage());
		}

		@Override
		public boolean isFinal() {
			return this.delegate.isFinal();
		}

		@Override
		public boolean isPackagePrivate() {
			return this.delegate.isPackagePrivate();
		}

		@Override
		public boolean isPrivate() {
			return this.delegate.isPrivate();
		}

		@Override
		public boolean isProtected() {
			return this.delegate.isProtected();
		}

		@Override
		public boolean isPublic() {
			return this.delegate.isPublic();
		}

		@Override
		public boolean isStatic() {
			return this.delegate.isStatic();
		}

		@Override
		public int modifierSpecifier() {
			return this.delegate.modifierSpecifier();
		}

		@Override
		public String modifiers() {
			return this.delegate.modifiers();
		}

		@Override
		public String qualifiedName() {
			return this.delegate.qualifiedName();
		}

		@Override
		public String commentText() {
			return this.delegate.commentText();
		}

		@Override
		public int compareTo(Object obj) {
			return this.delegate.compareTo(unwrap(obj));
		}

		@Override
		public Tag[] firstSentenceTags() {
			return this.delegate.firstSentenceTags();
		}

		@Override
		public String getRawCommentText() {
			return this.delegate.getRawCommentText();
		}

		@Override
		public Tag[] inlineTags() {
			return this.delegate.inlineTags();
		}

		@Override
		public boolean isAnnotationType() {
			return this.delegate.isAnnotationType();
		}

		@Override
		public boolean isAnnotationTypeElement() {
			return this.delegate.isAnnotationTypeElement();
		}

		@Override
		public boolean isClass() {
			return this.delegate.isClass();
		}

		@Override
		public boolean isConstructor() {
			return this.delegate.isConstructor();
		}

		@Override
		public boolean isEnum() {
			return this.delegate.isEnum();
		}

		@Override
		public boolean isEnumConstant() {
			return this.delegate.isEnumConstant();
		}

		@Override
		public boolean isError() {
			return this.delegate.isError();
		}

		@Override
		public boolean isException() {
			return this.delegate.isException();
		}

		@Override
		public boolean isField() {
			return this.delegate.isField();
		}

		@Override
		public boolean isIncluded() {
			return this.delegate.isIncluded();
		}

		@Override
		public boolean isInterface() {
			return this.delegate.isInterface();
		}

		@Override
		public boolean isMethod() {
			return this.delegate.isMethod();
		}

		@Override
		public boolean isOrdinaryClass() {
			return this.delegate.isOrdinaryClass();
		}

		@Override
		public String name() {
			return this.delegate.name();
		}

		@Override
		public SourcePosition position() {
			return this.delegate.position();
		}

		@Override
		public SeeTag[] seeTags() {
			return this.delegate.seeTags();
		}

		@Override
		public void setRawCommentText(String text) {
			this.delegate.setRawCommentText(text);
			Reflect.copyFields(FieldDocImpl.class, this, this.delegate);
		}

		@Override
		public Tag[] tags() {
			return this.delegate.tags();
		}

		@Override
		public Tag[] tags(String arg0) {
			return this.delegate.tags(arg0);
		}

		@Override
		public Object constantValue() {
			return this.delegate.constantValue();
		}

		@Override
		public String constantValueExpression() {
			return this.delegate.constantValueExpression();
		}

		@Override
		public boolean isTransient() {
			return this.delegate.isTransient();
		}

		@Override
		public boolean isVolatile() {
			return this.delegate.isVolatile();
		}

		@Override
		public SerialFieldTag[] serialFieldTags() {
			return this.delegate.serialFieldTags();
		}

		@Override
		public Type type() {
			return this.delegate.type();
		}

		@Override
		protected long getFlags() {
			return Reflect.callFunc(this.delegate, FieldDocImpl.class, long.class, "getFlags", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected ClassSymbol getContainingClass() {
			return Reflect.callFunc(this.delegate, FieldDocImpl.class, ClassSymbol.class, "getContainingClass", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected int getModifiers() {
			return Reflect.callFunc(this.delegate, ProgramElementDocImpl.class, int.class, "getModifiers", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected String documentation() {
			return Reflect.callFunc(this.delegate, DocImpl.class, String.class, "documentation", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		public String toString() {
			return this.delegate.toString();
		}

	}

	/** Wrapper for ConstructorDoc.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	@SuppressWarnings("checkstyle:all")
	private class ConstructorDocWrapper extends ConstructorDocImpl implements Proxy<ConstructorDocImpl> {

		private final ConstructorDocImpl delegate;

		@SuppressWarnings("synthetic-access")
		ConstructorDocWrapper(ConstructorDocImpl delegate) {
			super(getEnv(delegate), (MethodSymbol) getSym(delegate));
			Reflect.copyFields(ConstructorDocImpl.class, this, delegate);
			this.delegate = delegate;
		}

		@Override
		public ConstructorDocImpl getDelegate() {
			return this.delegate;
		}

		@Override
		public boolean equals(Object obj) {
			return this.delegate.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.delegate.hashCode();
		}

		@Override
		public String flatSignature() {
			return this.delegate.flatSignature();
		}

		@Override
		public boolean isNative() {
			return this.delegate.isNative();
		}

		@Override
		public boolean isSynchronized() {
			return this.delegate.isSynchronized();
		}

		@Override
		public boolean isVarArgs() {
			return this.delegate.isVarArgs();
		}

		@Override
		public ParamTag[] paramTags() {
			return this.delegate.paramTags();
		}

		@Override
		public Parameter[] parameters() {
			return this.delegate.parameters();
		}

		@Override
		public Type receiverType() {
			return this.delegate.receiverType();
		}

		@Override
		public String signature() {
			return this.delegate.signature();
		}

		@Override
		public Type[] thrownExceptionTypes() {
			return this.delegate.thrownExceptionTypes();
		}

		@Override
		public ClassDoc[] thrownExceptions() {
			return wrap(this.delegate.thrownExceptions());
		}

		@Override
		public ThrowsTag[] throwsTags() {
			return this.delegate.throwsTags();
		}

		@Override
		public ParamTag[] typeParamTags() {
			return this.delegate.typeParamTags();
		}

		@Override
		public TypeVariable[] typeParameters() {
			return this.delegate.typeParameters();
		}

		@Override
		public boolean isSynthetic() {
			return this.delegate.isSynthetic();
		}

		@Override
		public AnnotationDesc[] annotations() {
			return this.delegate.annotations();
		}

		@Override
		public ClassDoc containingClass() {
			return wrap(this.delegate.containingClass());
		}

		@Override
		public PackageDoc containingPackage() {
			return wrap(this.delegate.containingPackage());
		}

		@Override
		public boolean isFinal() {
			return this.delegate.isFinal();
		}

		@Override
		public boolean isPackagePrivate() {
			return this.delegate.isPackagePrivate();
		}

		@Override
		public boolean isPrivate() {
			return this.delegate.isPrivate();
		}

		@Override
		public boolean isProtected() {
			return this.delegate.isProtected();
		}

		@Override
		public boolean isPublic() {
			return this.delegate.isPublic();
		}

		@Override
		public boolean isStatic() {
			return this.delegate.isStatic();
		}

		@Override
		public int modifierSpecifier() {
			return this.delegate.modifierSpecifier();
		}

		@Override
		public String modifiers() {
			return this.delegate.modifiers();
		}

		@Override
		public String qualifiedName() {
			return this.delegate.qualifiedName();
		}

		@Override
		public String commentText() {
			return this.delegate.commentText();
		}

		@Override
		public int compareTo(Object obj) {
			return this.delegate.compareTo(unwrap(obj));
		}

		@Override
		public Tag[] firstSentenceTags() {
			return this.delegate.firstSentenceTags();
		}

		@Override
		public String getRawCommentText() {
			return this.delegate.getRawCommentText();
		}

		@Override
		public Tag[] inlineTags() {
			return this.delegate.inlineTags();
		}

		@Override
		public boolean isAnnotationType() {
			return this.delegate.isAnnotationType();
		}

		@Override
		public boolean isAnnotationTypeElement() {
			return this.delegate.isAnnotationTypeElement();
		}

		@Override
		public boolean isClass() {
			return this.delegate.isClass();
		}

		@Override
		public boolean isConstructor() {
			return this.delegate.isConstructor();
		}

		@Override
		public boolean isEnum() {
			return this.delegate.isEnum();
		}

		@Override
		public boolean isEnumConstant() {
			return this.delegate.isEnumConstant();
		}

		@Override
		public boolean isError() {
			return this.delegate.isError();
		}

		@Override
		public boolean isException() {
			return this.delegate.isException();
		}

		@Override
		public boolean isField() {
			return this.delegate.isField();
		}

		@Override
		public boolean isIncluded() {
			return this.delegate.isIncluded();
		}

		@Override
		public boolean isInterface() {
			return this.delegate.isInterface();
		}

		@Override
		public boolean isMethod() {
			return this.delegate.isMethod();
		}

		@Override
		public boolean isOrdinaryClass() {
			return this.delegate.isOrdinaryClass();
		}

		@Override
		public String name() {
			return this.delegate.name();
		}

		@Override
		public SourcePosition position() {
			return this.delegate.position();
		}

		@Override
		public SeeTag[] seeTags() {
			return this.delegate.seeTags();
		}

		@Override
		public void setRawCommentText(String text) {
			this.delegate.setRawCommentText(text);
			Reflect.copyFields(ConstructorDocImpl.class, this, this.delegate);
		}

		@Override
		public Tag[] tags() {
			return this.delegate.tags();
		}

		@Override
		public Tag[] tags(String arg0) {
			return this.delegate.tags(arg0);
		}

		@Override
		protected long getFlags() {
			return Reflect.callFunc(this.delegate, ExecutableMemberDocImpl.class, long.class, "getFlags", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected ClassSymbol getContainingClass() {
			return Reflect.callFunc(this.delegate, ExecutableMemberDocImpl.class, ClassSymbol.class, "getContainingClass", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected String typeParametersString() {
			return Reflect.callFunc(this.delegate, ExecutableMemberDocImpl.class, String.class, "typeParametersString", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected int getModifiers() {
			return Reflect.callFunc(this.delegate, ProgramElementDocImpl.class, int.class, "getModifiers", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected String documentation() {
			return Reflect.callFunc(this.delegate, DocImpl.class, String.class, "documentation", new Class<?>[0]); //$NON-NLS-1$
		}

	}

	/** Wrapper for MethodDoc.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	@SuppressWarnings("checkstyle:all")
	private class MethodDocWrapper extends MethodDocImpl implements Proxy<MethodDocImpl> {

		private final MethodDocImpl delegate;

		@SuppressWarnings("synthetic-access")
		MethodDocWrapper(MethodDocImpl delegate) {
			super(getEnv(delegate), (MethodSymbol) getSym(delegate));
			Reflect.copyFields(MethodDocImpl.class, this, delegate);
			this.delegate = delegate;
		}

		@Override
		public MethodDocImpl getDelegate() {
			return this.delegate;
		}

		@Override
		public boolean equals(Object obj) {
			return this.delegate.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.delegate.hashCode();
		}

		@Override
		public String flatSignature() {
			return this.delegate.flatSignature();
		}

		@Override
		public boolean isNative() {
			return this.delegate.isNative();
		}

		@Override
		public boolean isSynchronized() {
			return this.delegate.isSynchronized();
		}

		@Override
		public boolean isVarArgs() {
			return this.delegate.isVarArgs();
		}

		@Override
		public ParamTag[] paramTags() {
			return this.delegate.paramTags();
		}

		@Override
		public Parameter[] parameters() {
			return this.delegate.parameters();
		}

		@Override
		public Type receiverType() {
			return this.delegate.receiverType();
		}

		@Override
		public String signature() {
			return this.delegate.signature();
		}

		@Override
		public Type[] thrownExceptionTypes() {
			return this.delegate.thrownExceptionTypes();
		}

		@Override
		public ClassDoc[] thrownExceptions() {
			return wrap(this.delegate.thrownExceptions());
		}

		@Override
		public ThrowsTag[] throwsTags() {
			return this.delegate.throwsTags();
		}

		@Override
		public ParamTag[] typeParamTags() {
			return this.delegate.typeParamTags();
		}

		@Override
		public TypeVariable[] typeParameters() {
			return this.delegate.typeParameters();
		}

		@Override
		public boolean isSynthetic() {
			return this.delegate.isSynthetic();
		}

		@Override
		public AnnotationDesc[] annotations() {
			return this.delegate.annotations();
		}

		@Override
		public ClassDoc containingClass() {
			return wrap(this.delegate.containingClass());
		}

		@Override
		public PackageDoc containingPackage() {
			return wrap(this.delegate.containingPackage());
		}

		@Override
		public boolean isFinal() {
			return this.delegate.isFinal();
		}

		@Override
		public boolean isPackagePrivate() {
			return this.delegate.isPackagePrivate();
		}

		@Override
		public boolean isPrivate() {
			return this.delegate.isPrivate();
		}

		@Override
		public boolean isProtected() {
			return this.delegate.isProtected();
		}

		@Override
		public boolean isPublic() {
			return this.delegate.isPublic();
		}

		@Override
		public boolean isStatic() {
			return this.delegate.isStatic();
		}

		@Override
		public int modifierSpecifier() {
			return this.delegate.modifierSpecifier();
		}

		@Override
		public String modifiers() {
			return this.delegate.modifiers();
		}

		@Override
		public String qualifiedName() {
			return this.delegate.qualifiedName();
		}

		@Override
		public String commentText() {
			return this.delegate.commentText();
		}

		@Override
		public int compareTo(Object obj) {
			return this.delegate.compareTo(unwrap(obj));
		}

		@Override
		public Tag[] firstSentenceTags() {
			return this.delegate.firstSentenceTags();
		}

		@Override
		public String getRawCommentText() {
			return this.delegate.getRawCommentText();
		}

		@Override
		public Tag[] inlineTags() {
			return this.delegate.inlineTags();
		}

		@Override
		public boolean isAnnotationType() {
			return this.delegate.isAnnotationType();
		}

		@Override
		public boolean isAnnotationTypeElement() {
			return this.delegate.isAnnotationTypeElement();
		}

		@Override
		public boolean isClass() {
			return this.delegate.isClass();
		}

		@Override
		public boolean isConstructor() {
			return this.delegate.isConstructor();
		}

		@Override
		public boolean isEnum() {
			return this.delegate.isEnum();
		}

		@Override
		public boolean isEnumConstant() {
			return this.delegate.isEnumConstant();
		}

		@Override
		public boolean isError() {
			return this.delegate.isError();
		}

		@Override
		public boolean isException() {
			return this.delegate.isException();
		}

		@Override
		public boolean isField() {
			return this.delegate.isField();
		}

		@Override
		public boolean isIncluded() {
			return this.delegate.isIncluded();
		}

		@Override
		public boolean isInterface() {
			return this.delegate.isInterface();
		}

		@Override
		public boolean isMethod() {
			return this.delegate.isMethod();
		}

		@Override
		public boolean isOrdinaryClass() {
			return this.delegate.isOrdinaryClass();
		}

		@Override
		public String name() {
			return this.delegate.name();
		}

		@Override
		public SourcePosition position() {
			return this.delegate.position();
		}

		@Override
		public SeeTag[] seeTags() {
			return this.delegate.seeTags();
		}

		@Override
		public void setRawCommentText(String text) {
			this.delegate.setRawCommentText(text);
			Reflect.copyFields(MethodDocImpl.class, this, this.delegate);
		}

		@Override
		public Tag[] tags() {
			return this.delegate.tags();
		}

		@Override
		public Tag[] tags(String arg0) {
			return this.delegate.tags(arg0);
		}

		@Override
		public boolean isAbstract() {
			return this.delegate.isAbstract();
		}

		@Override
		public boolean isDefault() {
			return this.delegate.isDefault();
		}

		@Override
		public ClassDoc overriddenClass() {
			return wrap(this.delegate.overriddenClass());
		}

		@Override
		public MethodDoc overriddenMethod() {
			return wrap(this.delegate.overriddenMethod());
		}

		@Override
		public Type overriddenType() {
			return this.delegate.overriddenType();
		}

		@Override
		public boolean overrides(MethodDoc method) {
			return this.delegate.overrides(unwrap(method));
		}

		@Override
		public Type returnType() {
			return this.delegate.returnType();
		}

		@Override
		public String toString() {
			return this.delegate.toString();
		}

		@Override
		protected long getFlags() {
			return Reflect.callFunc(this.delegate, ExecutableMemberDocImpl.class, long.class, "getFlags", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected ClassSymbol getContainingClass() {
			return Reflect.callFunc(this.delegate, ExecutableMemberDocImpl.class, ClassSymbol.class, "getContainingClass", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected String typeParametersString() {
			return Reflect.callFunc(this.delegate, ExecutableMemberDocImpl.class, String.class, "typeParametersString", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected int getModifiers() {
			return Reflect.callFunc(this.delegate, ProgramElementDocImpl.class, int.class, "getModifiers", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected String documentation() {
			return Reflect.callFunc(this.delegate, DocImpl.class, String.class, "documentation", new Class<?>[0]); //$NON-NLS-1$
		}

	}

	/** Wrapper for AnnotationTypeElementDoc.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	@SuppressWarnings("checkstyle:all")
	private class AnnotationTypeElementDocWrapper extends AnnotationTypeElementDocImpl implements Proxy<MethodDocImpl> {

		private final AnnotationTypeElementDocImpl delegate;

		@SuppressWarnings("synthetic-access")
		AnnotationTypeElementDocWrapper(AnnotationTypeElementDocImpl delegate) {
			super(getEnv(delegate), (MethodSymbol) getSym(delegate));
			Reflect.copyFields(AnnotationTypeElementDocImpl.class, this, delegate);
			this.delegate = delegate;
		}

		@Override
		public AnnotationTypeElementDocImpl getDelegate() {
			return this.delegate;
		}

		@Override
		public boolean equals(Object obj) {
			return this.delegate.equals(obj);
		}

		@Override
		public int hashCode() {
			return this.delegate.hashCode();
		}

		@Override
		public boolean isAbstract() {
			return this.delegate.isAbstract();
		}

		@Override
		public boolean isDefault() {
			return this.delegate.isDefault();
		}

		@Override
		public ClassDoc overriddenClass() {
			return wrap(this.delegate.overriddenClass());
		}

		@Override
		public MethodDoc overriddenMethod() {
			return wrap(this.delegate.overriddenMethod());
		}

		@Override
		public Type overriddenType() {
			return this.delegate.overriddenType();
		}

		@Override
		public boolean overrides(MethodDoc method) {
			return this.delegate.overrides(unwrap(method));
		}

		@Override
		public Type returnType() {
			return this.delegate.returnType();
		}

		@Override
		public String flatSignature() {
			return this.delegate.flatSignature();
		}

		@Override
		public boolean isNative() {
			return this.delegate.isNative();
		}

		@Override
		public boolean isSynchronized() {
			return this.delegate.isSynchronized();
		}

		@Override
		public boolean isVarArgs() {
			return this.delegate.isVarArgs();
		}

		@Override
		public ParamTag[] paramTags() {
			return this.delegate.paramTags();
		}

		@Override
		public Parameter[] parameters() {
			return this.delegate.parameters();
		}

		@Override
		public Type receiverType() {
			return this.delegate.receiverType();
		}

		@Override
		public String signature() {
			return this.delegate.signature();
		}

		@Override
		public Type[] thrownExceptionTypes() {
			return this.delegate.thrownExceptionTypes();
		}

		@Override
		public ClassDoc[] thrownExceptions() {
			return wrap(this.delegate.thrownExceptions());
		}

		@Override
		public ThrowsTag[] throwsTags() {
			return this.delegate.throwsTags();
		}

		@Override
		public ParamTag[] typeParamTags() {
			return this.delegate.typeParamTags();
		}

		@Override
		public TypeVariable[] typeParameters() {
			return this.delegate.typeParameters();
		}

		@Override
		public boolean isSynthetic() {
			return this.delegate.isSynthetic();
		}

		@Override
		public AnnotationDesc[] annotations() {
			return this.delegate.annotations();
		}

		@Override
		public ClassDoc containingClass() {
			return wrap(this.delegate.containingClass());
		}

		@Override
		public PackageDoc containingPackage() {
			return wrap(this.delegate.containingPackage());
		}

		@Override
		public boolean isFinal() {
			return this.delegate.isFinal();
		}

		@Override
		public boolean isPackagePrivate() {
			return this.delegate.isPackagePrivate();
		}

		@Override
		public boolean isPrivate() {
			return this.delegate.isPrivate();
		}

		@Override
		public boolean isProtected() {
			return this.delegate.isProtected();
		}

		@Override
		public boolean isPublic() {
			return this.delegate.isPublic();
		}

		@Override
		public boolean isStatic() {
			return this.delegate.isStatic();
		}

		@Override
		public int modifierSpecifier() {
			return this.delegate.modifierSpecifier();
		}

		@Override
		public String modifiers() {
			return this.delegate.modifiers();
		}

		@Override
		public String qualifiedName() {
			return this.delegate.qualifiedName();
		}

		@Override
		public String commentText() {
			return this.delegate.commentText();
		}

		@Override
		public int compareTo(Object obj) {
			return this.delegate.compareTo(unwrap(obj));
		}

		@Override
		public Tag[] firstSentenceTags() {
			return this.delegate.firstSentenceTags();
		}

		@Override
		public String getRawCommentText() {
			return this.delegate.getRawCommentText();
		}

		@Override
		public Tag[] inlineTags() {
			return this.delegate.inlineTags();
		}

		@Override
		public boolean isAnnotationType() {
			return this.delegate.isAnnotationType();
		}

		@Override
		public boolean isAnnotationTypeElement() {
			return this.delegate.isAnnotationTypeElement();
		}

		@Override
		public boolean isClass() {
			return this.delegate.isClass();
		}

		@Override
		public boolean isConstructor() {
			return this.delegate.isConstructor();
		}

		@Override
		public boolean isEnum() {
			return this.delegate.isEnum();
		}

		@Override
		public boolean isEnumConstant() {
			return this.delegate.isEnumConstant();
		}

		@Override
		public boolean isError() {
			return this.delegate.isError();
		}

		@Override
		public boolean isException() {
			return this.delegate.isException();
		}

		@Override
		public boolean isField() {
			return this.delegate.isField();
		}

		@Override
		public boolean isIncluded() {
			return this.delegate.isIncluded();
		}

		@Override
		public boolean isInterface() {
			return this.delegate.isInterface();
		}

		@Override
		public boolean isMethod() {
			return this.delegate.isMethod();
		}

		@Override
		public boolean isOrdinaryClass() {
			return this.delegate.isOrdinaryClass();
		}

		@Override
		public String name() {
			return this.delegate.name();
		}

		@Override
		public SourcePosition position() {
			return this.delegate.position();
		}

		@Override
		public SeeTag[] seeTags() {
			return this.delegate.seeTags();
		}

		@Override
		public void setRawCommentText(String text) {
			this.delegate.setRawCommentText(text);
			Reflect.copyFields(AnnotationTypeElementDocImpl.class, this, this.delegate);
		}

		@Override
		public Tag[] tags() {
			return this.delegate.tags();
		}

		@Override
		public Tag[] tags(String arg0) {
			return this.delegate.tags(arg0);
		}

		@Override
		public AnnotationValue defaultValue() {
			return this.delegate.defaultValue();
		}

		@Override
		public String toString() {
			return this.delegate.toString();
		}

		@Override
		protected long getFlags() {
			return Reflect.callFunc(this.delegate, ExecutableMemberDocImpl.class, long.class, "getFlags", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected ClassSymbol getContainingClass() {
			return Reflect.callFunc(this.delegate, ExecutableMemberDocImpl.class, ClassSymbol.class, "getContainingClass", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected String typeParametersString() {
			return Reflect.callFunc(this.delegate, ExecutableMemberDocImpl.class, String.class, "typeParametersString", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected int getModifiers() {
			return Reflect.callFunc(this.delegate, ProgramElementDocImpl.class, int.class, "getModifiers", new Class<?>[0]); //$NON-NLS-1$
		}

		@Override
		protected String documentation() {
			return Reflect.callFunc(this.delegate, DocImpl.class, String.class, "documentation", new Class<?>[0]); //$NON-NLS-1$
		}

	}

}
