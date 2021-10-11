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

package io.sarl.docs.doclet.j11.guicemodules;

import java.lang.ref.WeakReference;

import javax.inject.Singleton;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provides;
import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.internal.doclets.formats.html.HtmlConfiguration;
import jdk.javadoc.internal.doclets.formats.html.SubWriterHolderWriter;
import jdk.javadoc.internal.doclets.toolkit.BaseConfiguration;
import jdk.javadoc.internal.doclets.toolkit.WriterFactory;
import jdk.javadoc.internal.doclets.toolkit.util.DocPath;
import jdk.javadoc.internal.doclets.toolkit.util.DocPaths;

import io.sarl.docs.doclet.j11.SarlHtmlDoclet;
import io.sarl.docs.doclet.j11.config.SarlHtmlConfiguration;
import io.sarl.docs.doclet.j11.factories.AnnotationTypeFieldWriterFactory;
import io.sarl.docs.doclet.j11.factories.AnnotationTypeOptionalMemberWriterFactory;
import io.sarl.docs.doclet.j11.factories.AnnotationTypeRequiredMemberWriterFactory;
import io.sarl.docs.doclet.j11.factories.AnnotationTypeWriterFactory;
import io.sarl.docs.doclet.j11.factories.ClassWriterFactory;
import io.sarl.docs.doclet.j11.factories.ConstructorWriterFactory;
import io.sarl.docs.doclet.j11.factories.DeprecatedListWriterFactory;
import io.sarl.docs.doclet.j11.factories.FieldWriterFactory;
import io.sarl.docs.doclet.j11.factories.LinkFactoryFactory;
import io.sarl.docs.doclet.j11.factories.MethodWriterFactory;
import io.sarl.docs.doclet.j11.factories.PackageTreeWriterFactory;
import io.sarl.docs.doclet.j11.factories.PackageWriterFactory;
import io.sarl.docs.doclet.j11.factories.TreeWriterFactory;
import io.sarl.docs.doclet.j11.factories.UtilsFactory;
import io.sarl.docs.doclet.j11.links.SarlLinkFactory;
import io.sarl.docs.doclet.j11.utils.SarlJavadocUtils;
import io.sarl.docs.doclet.j11.writers.SarlAnnotationTypeFieldWriter;
import io.sarl.docs.doclet.j11.writers.SarlAnnotationTypeOptionalMemberWriter;
import io.sarl.docs.doclet.j11.writers.SarlAnnotationTypeRequiredMemberWriter;
import io.sarl.docs.doclet.j11.writers.SarlAnnotationWriter;
import io.sarl.docs.doclet.j11.writers.SarlClassWriter;
import io.sarl.docs.doclet.j11.writers.SarlConstructorWriter;
import io.sarl.docs.doclet.j11.writers.SarlDeprecatedListWriter;
import io.sarl.docs.doclet.j11.writers.SarlFieldWriter;
import io.sarl.docs.doclet.j11.writers.SarlMethodWriter;
import io.sarl.docs.doclet.j11.writers.SarlPackageTreeWriter;
import io.sarl.docs.doclet.j11.writers.SarlPackageWriter;
import io.sarl.docs.doclet.j11.writers.SarlTreeWriter;
import io.sarl.docs.doclet.j11.writers.SarlWriterFactory;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Guice module for SARL Java Doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlJavadocModule extends AbstractModule {

	private final WeakReference<Doclet> doclet;

	/** Constructor.
	 *
	 * @param parent the parent doclet.
	 */
	public SarlJavadocModule(Doclet parent) {
		this.doclet = new WeakReference<>(parent);
	}

	/** Provide the writer factory.
	 *
	 * @param configuration the base configuration.
	 * @param classWriterFactory the factory of class writer.
	 * @param annotationTypeWriterFactory the factory of annotation type writer.
	 * @param fieldWriterFactory the factory of field writer.
	 * @param annotationTypeFieldWriterFactory the factory of annotation type's field writer.
	 * @param annotationTypeOptionalMemberWriterFactory the factory of annotation type's optional member writer.
	 * @param annotationTypeRequiredMemberWriterFactory the factory of annotation type's required member writer.
	 * @param methodWriterFactory the factory of method writer.
	 * @param constructorWriterFactory the factory of constructor writer.
	 * @param packageWriterFactory the factory of package summary.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public WriterFactory providesWriterFactory(HtmlConfiguration configuration,
			ClassWriterFactory classWriterFactory,
			AnnotationTypeWriterFactory annotationTypeWriterFactory,
			FieldWriterFactory fieldWriterFactory,
			AnnotationTypeFieldWriterFactory annotationTypeFieldWriterFactory,
			AnnotationTypeOptionalMemberWriterFactory annotationTypeOptionalMemberWriterFactory,
			AnnotationTypeRequiredMemberWriterFactory annotationTypeRequiredMemberWriterFactory,
			MethodWriterFactory methodWriterFactory,
			ConstructorWriterFactory constructorWriterFactory,
			PackageWriterFactory packageWriterFactory) {
		return new SarlWriterFactory(configuration,
				classWriterFactory, annotationTypeWriterFactory, fieldWriterFactory,
				annotationTypeFieldWriterFactory, annotationTypeOptionalMemberWriterFactory,
				annotationTypeRequiredMemberWriterFactory, methodWriterFactory, constructorWriterFactory,
				packageWriterFactory);
	}

	/** Provide the class writer factory.
	 *
	 * @param access accessor to the SARL keywords.
	 * @param linkFactory the factory of links.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public ClassWriterFactory providesClassWriterFactory(SARLGrammarKeywordAccess access,
			LinkFactoryFactory linkFactory) {
		return (configuration, typeElement, classTree) -> {
			return new SarlClassWriter(configuration, typeElement, classTree, access, linkFactory);
		};
	}

	/** Provide the annotation type writer factory.
	 *
	 * @param access accessor to the SARL keywords.
	 * @param linkFactory the factory of links.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public AnnotationTypeWriterFactory providesAnnotationTypeWriterFactory(SARLGrammarKeywordAccess access,
			LinkFactoryFactory linkFactory) {
		return (configuration, typeElement) -> {
			return new SarlAnnotationWriter(configuration, typeElement, access, linkFactory);
		};
	}

	/** Provide the field writer factory.
	 *
	 * @param access accessor to the SARL keywords.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public FieldWriterFactory providesFieldWriterFactory(SARLGrammarKeywordAccess access) {
		return container -> {
			return new SarlFieldWriter((SubWriterHolderWriter) container, container.getTypeElement(), access);
		};
	}

	/** Provide the annotation type's field writer factory.
	 *
	 * @param access accessor to the SARL keywords.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public AnnotationTypeFieldWriterFactory providesAnnotationTypeFieldWriterFactory(SARLGrammarKeywordAccess access) {
		return (writer, typeElement) -> {
			return new SarlAnnotationTypeFieldWriter((SubWriterHolderWriter) writer, typeElement, access);
		};
	}

	/** Provide the annotation type's optional member writer factory.
	 *
	 * @param access accessor to the SARL keywords.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public AnnotationTypeOptionalMemberWriterFactory providesAnnotationTypeOptionalMemberWriterFactory(SARLGrammarKeywordAccess access) {
		return (writer, typeElement) -> {
			return new SarlAnnotationTypeOptionalMemberWriter((SubWriterHolderWriter) writer, typeElement, access);
		};
	}

	/** Provide the annotation type's required member writer factory.
	 *
	 * @param access accessor to the SARL keywords.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public AnnotationTypeRequiredMemberWriterFactory providesAnnotationTypeRequiredMemberWriterFactory(SARLGrammarKeywordAccess access) {
		return (writer, typeElement) -> {
			return new SarlAnnotationTypeRequiredMemberWriter((SubWriterHolderWriter) writer, typeElement, access);
		};
	}

	/** Provide the method writer factory.
	 *
	 * @param access accessor to the SARL keywords.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public MethodWriterFactory providesMethodWriterFactory(SARLGrammarKeywordAccess access) {
		return container -> {
			return new SarlMethodWriter((SubWriterHolderWriter) container, container.getTypeElement(), access);
		};
	}

	/** Provide the constructor writer factory.
	 *
	 * @param access accessor to the SARL keywords.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public ConstructorWriterFactory providesConstructorWriterFactory(SARLGrammarKeywordAccess access) {
		return container -> {
			return new SarlConstructorWriter((SubWriterHolderWriter) container, container.getTypeElement(), access);
		};
	}

	/** Provide the HTML doclet for SARL.
	 *
	 * @param injector the current injector.
	 * @return the doclet.
	 */
	@Provides
	@Singleton
	public SarlHtmlDoclet providesSarlHtmlDoclet(Injector injector) {
		return new SarlHtmlDoclet(this.doclet.get(), injector);
	}

	/** Provide the HTML configuration for SARL.
	 *
	 * @return the configuration.
	 */
	@Provides
	@Singleton
	public SarlHtmlConfiguration providesSarlHtmlConfiguration(SarlHtmlDoclet doclet) {
		return doclet.getConfiguration();
	}

	/** Provide the HTML configuration.
	 *
	 * @return the configuration.
	 */
	@Provides
	@Singleton
	public HtmlConfiguration providesHtmlConfiguration(SarlHtmlDoclet doclet) {
		return doclet.getConfiguration();
	}

	/** Provide the HTML configuration.
	 *
	 * @return the configuration.
	 */
	@Provides
	@Singleton
	public BaseConfiguration providesBaseConfiguration(SarlHtmlDoclet doclet) {
		return doclet.getConfiguration();
	}

	/** Provide the factory of link factories.
	 *
	 * @param access accessor to the SARL keywords.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public LinkFactoryFactory providesLinkFactoryFactory(SARLGrammarKeywordAccess access) {
		return it -> {
			return new SarlLinkFactory(it, access);
		};
	}

	/** Provide the factory of link factories.
	 *
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public PackageWriterFactory providesPackageWriterFactory() {
		return (configuration, packageElement) -> {
			return new SarlPackageWriter(configuration, packageElement);
		};
	}

	/** Provide the factory of link factories.
	 *
	 * @param access accessor to the SARL keywords.
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public UtilsFactory providesUtilsFactory(SARLGrammarKeywordAccess access) {
		return it -> {
			return new SarlJavadocUtils(it, access);
		};
	}

	/** Provide the factory of writer of deprecated feature page.
	 *
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public DeprecatedListWriterFactory providesDeprecatedListWriterFactory() {
		return (configuration, filename) -> {
			return new SarlDeprecatedListWriter(configuration, filename);
		};
	}

	/** Provide the factory of class hierarchy.
	 *
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public TreeWriterFactory providesTreeWriterFactory() {
		return (configuration, classtree) -> {
			return new SarlTreeWriter(configuration, DocPaths.OVERVIEW_TREE, classtree);
		};
	}

	/** Provide the factory of package's class hierarchy.
	 *
	 * @return the factory.
	 */
	@Provides
	@Singleton
	public PackageTreeWriterFactory providesPackageTreeWriterFactory() {
		return (configuration, pkg, noDeprecated) -> {
	        final DocPath path = configuration.docPaths.forPackage(pkg).resolve(DocPaths.PACKAGE_TREE);
			return new SarlPackageTreeWriter(configuration, path, pkg);
		};
	}

}
