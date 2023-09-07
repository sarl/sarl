/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2.guice;

import java.util.HashSet;
import java.util.Set;

import javax.inject.Provider;
import javax.inject.Singleton;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.google.inject.MembersInjector;
import com.google.inject.Provides;
import com.google.inject.TypeLiteral;
import com.google.inject.name.Names;
import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.Taglet;

import io.sarl.docs.doclet2.framework.ApidocExcluder;
import io.sarl.docs.doclet2.framework.CustomTagParser;
import io.sarl.docs.doclet2.framework.CustomTagParserImpl;
import io.sarl.docs.doclet2.framework.DefaultApidocExcluder;
import io.sarl.docs.doclet2.framework.DocUtils;
import io.sarl.docs.doclet2.framework.DocUtilsImpl;
import io.sarl.docs.doclet2.framework.DocumentationRepository;
import io.sarl.docs.doclet2.framework.DocumentationRepositoryImpl;
import io.sarl.docs.doclet2.framework.ElementFilter;
import io.sarl.docs.doclet2.framework.ElementUtils;
import io.sarl.docs.doclet2.framework.ElementUtilsImpl;
import io.sarl.docs.doclet2.framework.ExternalLinkManager;
import io.sarl.docs.doclet2.framework.ExternalLinkManagerImpl;
import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.framework.SarlDocletEnvironmentImpl;
import io.sarl.docs.doclet2.framework.SarlTagletFactory;
import io.sarl.docs.doclet2.framework.StandardElementFilter;
import io.sarl.docs.doclet2.framework.TagletManager;
import io.sarl.docs.doclet2.framework.TagletManagerImpl;
import io.sarl.docs.doclet2.framework.TypeHierarchy;
import io.sarl.docs.doclet2.framework.TypeHierarchyImpl;
import io.sarl.docs.doclet2.framework.TypeRepository;
import io.sarl.docs.doclet2.framework.TypeRepositoryImpl;
import io.sarl.docs.doclet2.html.SarlHtmlDoclet;
import io.sarl.docs.doclet2.html.frames.AllTypesFrameGenerator;
import io.sarl.docs.doclet2.html.frames.AllTypesFrameGeneratorImpl;
import io.sarl.docs.doclet2.html.frames.HtmlIndexGenerator;
import io.sarl.docs.doclet2.html.frames.HtmlIndexGeneratorImpl;
import io.sarl.docs.doclet2.html.frames.OverviewFrameGenerator;
import io.sarl.docs.doclet2.html.frames.OverviewFrameGeneratorImpl;
import io.sarl.docs.doclet2.html.framework.BlockTagSorter;
import io.sarl.docs.doclet2.html.framework.BlockTagSorterImpl;
import io.sarl.docs.doclet2.html.framework.DefaultDocletOptions;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.HtmlAccessor;
import io.sarl.docs.doclet2.html.framework.HtmlAccessorImpl;
import io.sarl.docs.doclet2.html.framework.HtmlFactory;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryImpl;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.NavigationImpl;
import io.sarl.docs.doclet2.html.framework.PathBuilder;
import io.sarl.docs.doclet2.html.framework.PathBuilderImpl;
import io.sarl.docs.doclet2.html.indexes.IndexGenerator;
import io.sarl.docs.doclet2.html.indexes.IndexGeneratorImpl;
import io.sarl.docs.doclet2.html.raw.RawModuleListGenerator;
import io.sarl.docs.doclet2.html.raw.RawModuleListGeneratorImpl;
import io.sarl.docs.doclet2.html.raw.RawPackageListGenerator;
import io.sarl.docs.doclet2.html.raw.RawPackageListGeneratorImpl;
import io.sarl.docs.doclet2.html.summaries.AllTypeSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.AllTypeSummaryGeneratorImpl;
import io.sarl.docs.doclet2.html.summaries.DeprecatedListGenerator;
import io.sarl.docs.doclet2.html.summaries.DeprecatedListGeneratorImpl;
import io.sarl.docs.doclet2.html.summaries.ModuleSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.ModuleSummaryGeneratorImpl;
import io.sarl.docs.doclet2.html.summaries.OverviewSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.OverviewSummaryGeneratorImpl;
import io.sarl.docs.doclet2.html.summaries.PackageSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.PackageSummaryGeneratorImpl;
import io.sarl.docs.doclet2.html.summaries.PackageTreeSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.PackageTreeSummaryGeneratorImpl;
import io.sarl.docs.doclet2.html.summaries.TreeSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.TreeSummaryGeneratorImpl;
import io.sarl.docs.doclet2.html.taglets.block.AuthorTaglet;
import io.sarl.docs.doclet2.html.taglets.block.DeprecatedTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ExceptionTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ExcludeFromApidocTaglet;
import io.sarl.docs.doclet2.html.taglets.block.FiresTaglet;
import io.sarl.docs.doclet2.html.taglets.block.GeneratedTaglet;
import io.sarl.docs.doclet2.html.taglets.block.HiddenTaglet;
import io.sarl.docs.doclet2.html.taglets.block.MavenArtifactIdTaglet;
import io.sarl.docs.doclet2.html.taglets.block.MavenGroupIdTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ModelTaglet;
import io.sarl.docs.doclet2.html.taglets.block.OptionalParamTaglet;
import io.sarl.docs.doclet2.html.taglets.block.OrderedTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ParamTaglet;
import io.sarl.docs.doclet2.html.taglets.block.PrivateApiTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ProvidesTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ReturnTaglet;
import io.sarl.docs.doclet2.html.taglets.block.SeeTaglet;
import io.sarl.docs.doclet2.html.taglets.block.SinceTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ThrowsTaglet;
import io.sarl.docs.doclet2.html.taglets.block.UsesTaglet;
import io.sarl.docs.doclet2.html.taglets.block.VersionTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.CodeTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.CommentTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.DocRootTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.InheritDocTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.LinkTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.LiteralTaglet;
import io.sarl.docs.doclet2.html.taglets.inline.ValueTaglet;
import io.sarl.docs.doclet2.html.types.SarlTypeDocumentationGeneratorSelector;
import io.sarl.docs.doclet2.html.types.TypeDocumentationGeneratorSelector;
import io.sarl.docs.doclet2.html.types.aop.AgentDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.aop.AgentDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.aop.BehaviorDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.aop.BehaviorDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.aop.CapacityDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.aop.CapacityDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.aop.EventDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.aop.EventDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.aop.SkillDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.aop.SkillDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.oop.AnnotationDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.oop.AnnotationDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.oop.ClassDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.oop.ClassDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.oop.EnumerationDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.oop.EnumerationDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.oop.InterfaceDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.oop.InterfaceDocumentationGeneratorImpl;
import io.sarl.docs.doclet2.html.types.oop.RecordDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.oop.RecordDocumentationGeneratorImpl;

/** Guice module for SARL Java Doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlJavadocModule extends AbstractModule {

	private final Doclet doclet;

	/** Constructor.
	 *
	 * @param parent the parent doclet.
	 */
	public SarlJavadocModule(Doclet parent) {
		this.doclet = parent;
	}

	/** Utility function for binding singleton.
	 *
	 * @param <T> the source type.
	 * @param source the source type.
	 * @param target the target type.
	 */
	protected <T> void bindSingleton(Class<T> source, Class<? extends T> target) {
		binder().bind(source).to(target).in(Singleton.class);
	}

	/** Utility function for binding (not singleton).
	 *
	 * @param <T> the source type.
	 * @param source the source type.
	 * @param target the target type.
	 */
	protected <T> void bind(Class<T> source, Class<? extends T> target) {
		binder().bind(source).to(target);
	}

	private <T extends Taglet> Provider<T> createProvider(Class<T> type) {
		final MembersInjector<T> minjector = binder().getMembersInjector(type);
		final Provider<T> provider = () -> {
			try {
				final T taglet = type.getConstructor().newInstance();
				minjector.injectMembers(taglet);
				return taglet;
			} catch (Throwable ex) {
				throw new RuntimeException(ex);
			}
		};
		return provider;
	}

	@Override
	protected void configure() {
		bindSingleton(DocletOptions.class, DefaultDocletOptions.class);
		bindSingleton(ElementFilter.class, StandardElementFilter.class);
		bindSingleton(TypeDocumentationGeneratorSelector.class, SarlTypeDocumentationGeneratorSelector.class);
		bindSingleton(PathBuilder.class, PathBuilderImpl.class);
		bindSingleton(HtmlAccessor.class, HtmlAccessorImpl.class);
		bindSingleton(HtmlFactory.class, HtmlFactoryImpl.class);
		bind(Navigation.class, NavigationImpl.class);
		bindSingleton(ElementUtils.class, ElementUtilsImpl.class);
		bindSingleton(DocUtils.class, DocUtilsImpl.class);
		bindSingleton(ExternalLinkManager.class, ExternalLinkManagerImpl.class);
		bindSingleton(TypeHierarchy.class, TypeHierarchyImpl.class);
		bindSingleton(TypeRepository.class, TypeRepositoryImpl.class);
		bindSingleton(TagletManager.class, TagletManagerImpl.class);
		bindSingleton(CustomTagParser.class, CustomTagParserImpl.class);
		bindSingleton(DocumentationRepository.class, DocumentationRepositoryImpl.class);
		bindSingleton(BlockTagSorter.class, BlockTagSorterImpl.class);
		//
		bindSingleton(AgentDocumentationGenerator.class, AgentDocumentationGeneratorImpl.class);
		bindSingleton(BehaviorDocumentationGenerator.class, BehaviorDocumentationGeneratorImpl.class);
		bindSingleton(SkillDocumentationGenerator.class, SkillDocumentationGeneratorImpl.class);
		bindSingleton(EventDocumentationGenerator.class, EventDocumentationGeneratorImpl.class);
		bindSingleton(CapacityDocumentationGenerator.class, CapacityDocumentationGeneratorImpl.class);
		//
		bindSingleton(ClassDocumentationGenerator.class, ClassDocumentationGeneratorImpl.class);
		bindSingleton(RecordDocumentationGenerator.class, RecordDocumentationGeneratorImpl.class);
		bindSingleton(InterfaceDocumentationGenerator.class, InterfaceDocumentationGeneratorImpl.class);
		bindSingleton(EnumerationDocumentationGenerator.class, EnumerationDocumentationGeneratorImpl.class);
		bindSingleton(AnnotationDocumentationGenerator.class, AnnotationDocumentationGeneratorImpl.class);
		//
		bindSingleton(OverviewSummaryGenerator.class, OverviewSummaryGeneratorImpl.class);
		bindSingleton(AllTypeSummaryGenerator.class, AllTypeSummaryGeneratorImpl.class);
		bindSingleton(ModuleSummaryGenerator.class, ModuleSummaryGeneratorImpl.class);
		bindSingleton(PackageSummaryGenerator.class, PackageSummaryGeneratorImpl.class);
		bindSingleton(DeprecatedListGenerator.class, DeprecatedListGeneratorImpl.class);
		bindSingleton(TreeSummaryGenerator.class, TreeSummaryGeneratorImpl.class);
		bindSingleton(PackageTreeSummaryGenerator.class, PackageTreeSummaryGeneratorImpl.class);
		bindSingleton(IndexGenerator.class, IndexGeneratorImpl.class);
		bindSingleton(HtmlIndexGenerator.class, HtmlIndexGeneratorImpl.class);
		bindSingleton(OverviewFrameGenerator.class, OverviewFrameGeneratorImpl.class);
		bindSingleton(AllTypesFrameGenerator.class, AllTypesFrameGeneratorImpl.class);
		bindSingleton(RawModuleListGenerator.class, RawModuleListGeneratorImpl.class);
		bindSingleton(RawPackageListGenerator.class, RawPackageListGeneratorImpl.class);
		//
		bindTaglets(
			"author", AuthorTaglet.class, //$NON-NLS-1$
			"code", CodeTaglet.class, //$NON-NLS-1$
			"comment", CommentTaglet.class, //$NON-NLS-1$
			"deprecated", DeprecatedTaglet.class, //$NON-NLS-1$
			"docroot", DocRootTaglet.class, //$NON-NLS-1$
			"exception", ExceptionTaglet.class, //$NON-NLS-1$
			"excludefromapidoc", ExcludeFromApidocTaglet.class, //$NON-NLS-1$
			"fires", FiresTaglet.class, //$NON-NLS-1$
			"generated", GeneratedTaglet.class, //$NON-NLS-1$
			"hidden", HiddenTaglet.class, //$NON-NLS-1$
			"inheritdoc", InheritDocTaglet.class, //$NON-NLS-1$
			"link", LinkTaglet.class, //$NON-NLS-1$
			"linkplain", LinkTaglet.class, //$NON-NLS-1$
			"literal", LiteralTaglet.class, //$NON-NLS-1$
			"mavenartifactid", MavenArtifactIdTaglet.class, //$NON-NLS-1$
			"mavengroupid", MavenGroupIdTaglet.class, //$NON-NLS-1$
			"model", ModelTaglet.class, //$NON-NLS-1$
			"optionalparam", OptionalParamTaglet.class, //$NON-NLS-1$
			"ordered", OrderedTaglet.class, //$NON-NLS-1$
			"param", ParamTaglet.class, //$NON-NLS-1$
			"privateapi", PrivateApiTaglet.class, //$NON-NLS-1$
			"provides", ProvidesTaglet.class, //$NON-NLS-1$
			"return", ReturnTaglet.class, //$NON-NLS-1$
			"see", SeeTaglet.class, //$NON-NLS-1$
			"since", SinceTaglet.class, //$NON-NLS-1$
			"throws", ThrowsTaglet.class, //$NON-NLS-1$
			"uses", UsesTaglet.class, //$NON-NLS-1$
			"value", ValueTaglet.class, //$NON-NLS-1$
			"version", VersionTaglet.class); //$NON-NLS-1$
	}

	/** Replies the HTML doclet.
	 *
	 * @param injector the injector.
	 * @return the doclet.
	 */
	@Provides
	@Singleton
	public Doclet providesDoclet(Injector injector) {
		final SarlHtmlDoclet doclet = new SarlHtmlDoclet(this.doclet);
		injector.injectMembers(doclet);
		return doclet;
	}

	/** Replies the SARL doclet environment.
	 *
	 * @param injector the injector.
	 * @return the environment.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public SarlDocletEnvironment providesEnvironment(Injector injector) {
		final SarlDocletEnvironment env = new SarlDocletEnvironmentImpl();
		injector.injectMembers(env);
		return env;
	}

	/** Replies the API excluder.
	 *
	 * @param injector the injector.
	 * @param environment the provider of the generation environment.
	 * @return the excluder.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public ApidocExcluder providesApidocExcluder(Injector injector, Provider<SarlDocletEnvironment> environment) {
		final ApidocExcluder excluder = new DefaultApidocExcluder(environment);
		injector.injectMembers(excluder);
		return excluder;
	}

	/** Replies the SARL taglet factory.
	 *
	 * @param injector the injector.
	 * @return the excluder.
	 */
	@Provides
	@Singleton
	public SarlTagletFactory providesSarlTagletFactory(Injector injector) {
		return new SarlTagletFactory() {
			@Override
			public <T extends Taglet> T newTaglet(Class<T> type) {
				final Provider<T> provider = createProvider(type);
				return provider.get();
			}
		};
	}

	/** Utility function for binding taglet.
	 *
	 * @param pairs the pairs of bindings, composed by the name of the tag and the target type.
	 */
	@SuppressWarnings("unchecked")
	protected void bindTaglets(Object... pairs) {
		final Set<Provider<? extends Taglet>> typeSet = new HashSet<>(); 
		for (int i = 0; i < pairs.length; i += 2) {
			final Class<? extends Taglet> type = (Class<? extends Taglet>) pairs[i + 1];
			typeSet.add(createProvider(type));
		}
		final TypeLiteral<Set<Provider<? extends Taglet>>> typeLiteral = new TypeLiteral<>() {
			//
		};
		final Key<Set<Provider<? extends Taglet>>> collectionKey = Key.get(typeLiteral, Names.named("registered-taglets")); //$NON-NLS-1$
		binder().bind(collectionKey).toInstance(typeSet);
	}

}
