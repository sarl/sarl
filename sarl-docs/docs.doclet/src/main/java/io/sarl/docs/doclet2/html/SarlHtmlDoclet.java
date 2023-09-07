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
 *
 *------- FORKED SOURCE CODE:
 *
 * THIS CODE IS FORKED FROM JDK.JAVADOC INTERNAL PACKAGE AND ADAPTED TO THE SARL PURPOSE.
 * THE FORK WAS NECESSARY BECAUSE IT IS IMPOSSIBLE TO SUBCLASS THE TYPES FOR THE.
 * STANDARD HTML DOCLET THAT IS PROVIDED BY JDK.JAVADOC MODULE.
 *
 * Copyright (c) 2003, 2021, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package io.sarl.docs.doclet2.html;

import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.AUTHOR_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.BOTTOM_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.CHARSET_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.COPYRIGHT_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.DIRECTORY_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.DOCENCODING_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.DOCTITLE_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.FAKE_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.GROUP_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.HTMLCOMMENTS_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.LINK_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.NODEPRECATED_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.NOSINCE_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.OFFLINE_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.TAGLET_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.TAG_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.TITLE_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.VERSION_OPTION;
import static io.sarl.docs.doclet2.html.SarlHtmlDocletOptions.WINDOWTITLE_OPTION;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Consumer;
import java.util.regex.Pattern;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Provider;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic.Kind;

import com.google.common.collect.Iterables;
import com.google.common.io.Resources;
import com.google.inject.MembersInjector;
import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.Reporter;
import jdk.javadoc.doclet.Taglet;
import org.eclipse.xtext.util.Files;
import org.eclipse.xtext.util.Strings;

import io.sarl.docs.doclet2.framework.AbstractDoclet;
import io.sarl.docs.doclet2.framework.CustomTag;
import io.sarl.docs.doclet2.framework.CustomTagLocation;
import io.sarl.docs.doclet2.framework.CustomTagParser;
import io.sarl.docs.doclet2.framework.ElementFilter;
import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.framework.SarlTagletFactory;
import io.sarl.docs.doclet2.framework.TypeHierarchy;
import io.sarl.docs.doclet2.framework.TypeRepository;
import io.sarl.docs.doclet2.html.frames.AllTypesFrameGenerator;
import io.sarl.docs.doclet2.html.frames.HtmlIndexGenerator;
import io.sarl.docs.doclet2.html.frames.OverviewFrameGenerator;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.indexes.IndexGenerator;
import io.sarl.docs.doclet2.html.raw.RawModuleListGenerator;
import io.sarl.docs.doclet2.html.raw.RawPackageListGenerator;
import io.sarl.docs.doclet2.html.summaries.AllTypeSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.DeprecatedListGenerator;
import io.sarl.docs.doclet2.html.summaries.ModuleSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.OverviewSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.PackageSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.PackageTreeSummaryGenerator;
import io.sarl.docs.doclet2.html.summaries.TreeSummaryGenerator;
import io.sarl.docs.doclet2.html.taglets.block.CustomTaglet;
import io.sarl.docs.doclet2.html.types.TypeDocumentationGenerator;
import io.sarl.docs.doclet2.html.types.TypeDocumentationGeneratorSelector;

/** SARL Doclet that is generated the HTML documentation.
 *
 * <p>This version of the SARL doc let is an adaptation of the
 * previous SARL doclet (for Java 8) to Java 11 and higher API.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class SarlHtmlDoclet extends AbstractDoclet {

	private static final String CSS_FOLDER_NAME = "css"; //$NON-NLS-1$
	
	private static final String JS_FOLDER_NAME = "js"; //$NON-NLS-1$

	private ElementFilter elementFilter;
	
	private TypeDocumentationGeneratorSelector typeGeneratorSelector;

	private IndexGenerator indexGenerator;

	private RawModuleListGenerator rawModuleListGenerator;

	private RawPackageListGenerator rawPackageListGenerator;

	private HtmlIndexGenerator htmlIndexGenerator;

	private OverviewFrameGenerator overviewFrameGenerator;

	private AllTypesFrameGenerator allTypesFrameGenerator;

	private OverviewSummaryGenerator overviewSummaryGenerator;

	private AllTypeSummaryGenerator allTypeSummaryGenerator;

	private ModuleSummaryGenerator moduleSummaryGenerator;

	private PackageSummaryGenerator packageSummaryGenerator;

	private DeprecatedListGenerator deprecatedListGenerator;

	private TreeSummaryGenerator treeSummaryGenerator;

	private PackageTreeSummaryGenerator packageTreeSummaryGenerator;

	private TypeHierarchy typeHierarchy;

	private TypeRepository typeRepository;

	private DocletOptions docletOptions;

	private SarlTagletFactory sarlTagletFactory;

	private MembersInjector<CustomTaglet> customTagletInjector;

	private Map<URL, Path> cssResources = null;
	
	private Map<URL, Path> jsResources = null;

	private Set<Provider<? extends Taglet>> registeredTaglets;

	private final Set<Option> options = Set.of(
			new Option(AUTHOR_OPTION, Messages.SarlHtmlDoclet_32) {
				@Override
				public boolean process(String option, List<String> arguments) {
					getDocletOptions().setAuthorTagsEnabled(true);
					return true;				
				}
			},
			new Option(BOTTOM_OPTION, Messages.SarlHtmlDoclet_33, Messages.SarlHtmlDoclet_20) {
				@Override
				public boolean process(String option, List<String> arguments) {
					if (arguments.size() >= 1) {
						getDocletOptions().setCopyrightText(arguments.get(0));
						return true;
					}
					return false;
				}
			},
			new Option(CHARSET_OPTION, Messages.SarlHtmlDoclet_34, Messages.SarlHtmlDoclet_35) {
				@Override
				public boolean process(String option, List<String> arguments) {
					if (arguments.size() >= 1) {
						getDocletOptions().setCharset(arguments.get(0));
						return true;
					}
					return false;
				}
			},
			new Option(COPYRIGHT_OPTION, Messages.SarlHtmlDoclet_19, Messages.SarlHtmlDoclet_20) {
				@Override
				public boolean process(String option, List<String> arguments) {
					if (arguments.size() >= 1) {
						getDocletOptions().setCopyrightText(arguments.get(0));
						return true;
					}
					return false;
				}
			},
			new Option(DIRECTORY_OPTION, Messages.SarlHtmlDoclet_0, Messages.SarlHtmlDoclet_1) {
				@Override
				public boolean process(String option, List<String> arguments) {
					getDocletOptions().setOutputDirectory(new File(arguments.get(0)).toPath());
					return true;				
				}
			},
			new Option(DOCENCODING_OPTION, Messages.SarlHtmlDoclet_36, Messages.SarlHtmlDoclet_35) {
				@Override
				public boolean process(String option, List<String> arguments) {
					if (arguments.size() >= 1) {
						getDocletOptions().setCharset(arguments.get(0));
						return true;
					}
					return false;
				}
			},
			new Option(DOCTITLE_OPTION, Messages.SarlHtmlDoclet_37, Messages.SarlHtmlDoclet_23) {
				@Override
				public boolean process(String option, List<String> arguments) {
					final StringBuilder b = new StringBuilder();
					boolean first = true;
					for (final String s : arguments) {
						if (first) {
							first = false;
						} else {
							b.append(" "); //$NON-NLS-1$
						}
						b.append(s);
					}
					getDocletOptions().setTitle(b.toString());
					return true;				
				}
			},
			new Option(FAKE_OPTION, Messages.SarlHtmlDoclet_3) {
				@Override
				public boolean process(String option, List<String> arguments) {
					getDocletOptions().setFakeOutput(true);
					return true;				
				}
			},
			new Option(GROUP_OPTION, MessageFormat.format(Messages.SarlHtmlDoclet_24, GROUP_OPTION), Messages.SarlHtmlDoclet_25, 2) {
				@Override
				public boolean process(String option, List<String> arguments) {
					if (arguments.size() >= 2) {
						final String heading = arguments.get(0);
						if (!Strings.isEmpty(heading)) {
							final String patterns = arguments.get(1);
							if (!Strings.isEmpty(patterns)) {
								final String[] groupPatterns = patterns.split(Pattern.quote(":")); //$NON-NLS-1$
								if (groupPatterns != null && groupPatterns.length > 0) {
									getDocletOptions().addGroup(heading, groupPatterns);
									return true;
								}
								getReporter().print(javax.tools.Diagnostic.Kind.ERROR,
										MessageFormat.format(Messages.SarlHtmlDoclet_28, heading, patterns));
							} else {
								getReporter().print(javax.tools.Diagnostic.Kind.ERROR,
										MessageFormat.format(Messages.SarlHtmlDoclet_27, heading));
							}
						} else {
							getReporter().print(javax.tools.Diagnostic.Kind.ERROR, Messages.SarlHtmlDoclet_26);
						}
					}
					return false;
				}
			},
			new Option(HTMLCOMMENTS_OPTION, Messages.SarlHtmlDoclet_18) {
				@Override
				public boolean process(String option, List<String> arguments) {
					getDocletOptions().setHtmlCommentsEnabled(true);
					return true;				
				}
			},
			new Option(LINK_OPTION, Messages.SarlHtmlDoclet_5, Messages.SarlHtmlDoclet_6) {
				@Override
				public boolean process(String option, List<String> arguments) {
					try {
						final URL url = new URL(arguments.get(0));
						getExternalLinkManager().addExternalLink(url);
					} catch (MalformedURLException ex) {
						throw new RuntimeException(ex);
					}
					return true;				
				}
			},
			new Option(NODEPRECATED_OPTION, Messages.SarlHtmlDoclet_29) {
				@Override
				public boolean process(String option, List<String> arguments) {
					getDocletOptions().setDeprecatedFeaturesEnabled(false);
					return true;				
				}
			},
			new Option(NOSINCE_OPTION, Messages.SarlHtmlDoclet_30) {
				@Override
				public boolean process(String option, List<String> arguments) {
					getDocletOptions().setSinceTagsEnabled(false);
					return true;				
				}
			},
			new Option(TAG_OPTION, Messages.SarlHtmlDoclet_8, Messages.SarlHtmlDoclet_9) {
				@Override
				public boolean process(String option, List<String> arguments) {
					final String tagName = arguments.get(0);
					if (!Strings.isEmpty(tagName)) {
						getDocletOptions().addUserTag(tagName);
					}
					return true;				
				}
			},
			new Option(TAGLET_OPTION, Messages.SarlHtmlDoclet_13, Messages.SarlHtmlDoclet_14) {
				@Override
				public boolean process(String option, List<String> arguments) {
					final String classname = arguments.get(0);
					if (!Strings.isEmpty(classname)) {
						try {
							final Class<?> tagletType = Class.forName(classname);
							if (Taglet.class.isAssignableFrom(tagletType)) {
								final Class<? extends Taglet> tagletTypeType = tagletType.asSubclass(Taglet.class);
								final Taglet taglet = SarlHtmlDoclet.this.getSarlTagletFactory().newTaglet(tagletTypeType);
								if (taglet != null) {
									SarlHtmlDoclet.this.getTagletManager().addTaglet(taglet, false);
								} else {
									getReporter().print(javax.tools.Diagnostic.Kind.ERROR, MessageFormat.format(Messages.SarlHtmlDoclet_17, classname));
								}
							} else {
								getReporter().print(javax.tools.Diagnostic.Kind.ERROR, MessageFormat.format(Messages.SarlHtmlDoclet_17, classname));
							}
						} catch (Throwable ex) {
							getReporter().print(javax.tools.Diagnostic.Kind.ERROR, MessageFormat.format(Messages.SarlHtmlDoclet_15, classname, ex.getLocalizedMessage()));
						}
					} else {
						getReporter().print(javax.tools.Diagnostic.Kind.ERROR, Messages.SarlHtmlDoclet_16);
					}
					return true;				
				}
			},
			new Option(TITLE_OPTION, Messages.SarlHtmlDoclet_22, Messages.SarlHtmlDoclet_23) {
				@Override
				public boolean process(String option, List<String> arguments) {
					final StringBuilder b = new StringBuilder();
					boolean first = true;
					for (final String s : arguments) {
						if (first) {
							first = false;
						} else {
							b.append(" "); //$NON-NLS-1$
						}
						b.append(s);
					}
					getDocletOptions().setTitle(b.toString());
					return true;				
				}
			},
			new Option(OFFLINE_OPTION, Messages.SarlHtmlDoclet_12) {
				@Override
				public boolean process(String option, List<String> arguments) {
					getDocletOptions().setOffline(true);
					return true;				
				}
			},
			new Option(VERSION_OPTION, Messages.SarlHtmlDoclet_31) {
				@Override
				public boolean process(String option, List<String> arguments) {
					getDocletOptions().setVersionTagsEnabled(true);
					return true;				
				}
			},
			new Option(WINDOWTITLE_OPTION, Messages.SarlHtmlDoclet_37, Messages.SarlHtmlDoclet_23) {
				@Override
				public boolean process(String option, List<String> arguments) {
					final StringBuilder b = new StringBuilder();
					boolean first = true;
					for (final String s : arguments) {
						if (first) {
							first = false;
						} else {
							b.append(" "); //$NON-NLS-1$
						}
						b.append(s);
					}
					getDocletOptions().setTitle(b.toString());
					return true;				
				}
			});

	/** Change the index generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setIndexGenerator(IndexGenerator generator) {
		this.indexGenerator = generator;
	}

	/** Replies the overview index generator.
	 *
	 * @return the generator.
	 */
	public IndexGenerator getIndexGenerator() {
		return this.indexGenerator;
	}

	/** Change the raw module list generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setRawModuleListGenerator(RawModuleListGenerator generator) {
		this.rawModuleListGenerator = generator;
	}

	/** Replies the raw module list generator.
	 *
	 * @return the generator.
	 */
	public RawModuleListGenerator getRawModuleListGenerator() {
		return this.rawModuleListGenerator;
	}

	/** Change the raw package list generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setRawPackageListGenerator(RawPackageListGenerator generator) {
		this.rawPackageListGenerator = generator;
	}

	/** Replies the raw package list generator.
	 *
	 * @return the generator.
	 */
	public RawPackageListGenerator getRawPackageListGenerator() {
		return this.rawPackageListGenerator;
	}

	/** Change the HTML main index generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setHtmlIndexGenerator(HtmlIndexGenerator generator) {
		this.htmlIndexGenerator = generator;
	}

	/** Replies the HTML main index generator.
	 *
	 * @return the generator.
	 */
	public HtmlIndexGenerator getHtmlIndexGenerator() {
		return this.htmlIndexGenerator;
	}

	/** Change the overview frame generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setOverviewFrameGenerator(OverviewFrameGenerator generator) {
		this.overviewFrameGenerator = generator;
	}

	/** Replies the overview frame generator.
	 *
	 * @return the generator.
	 */
	public OverviewFrameGenerator getOverviewFrameGenerator() {
		return this.overviewFrameGenerator;
	}

	/** Change all-types the overview frame generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setAllTypesFrameGenerator(AllTypesFrameGenerator generator) {
		this.allTypesFrameGenerator = generator;
	}

	/** Replies the all-types overview frame generator.
	 *
	 * @return the generator.
	 */
	public AllTypesFrameGenerator getAllTypesFrameGenerator() {
		return this.allTypesFrameGenerator;
	}

	/** Change the overview summary generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setOverviewSummaryGenerator(OverviewSummaryGenerator generator) {
		this.overviewSummaryGenerator = generator;
	}

	/** Replies the overview summary generator.
	 *
	 * @return the generator.
	 */
	public OverviewSummaryGenerator getOverviewSummaryGenerator() {
		return this.overviewSummaryGenerator;
	}

	/** Change the deprecated list generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setDeprecatedListGenerator(DeprecatedListGenerator generator) {
		this.deprecatedListGenerator = generator;
	}

	/** Replies the deprecated list generator.
	 *
	 * @return the generator.
	 */
	public DeprecatedListGenerator getDeprecatedListGenerator() {
		return this.deprecatedListGenerator;
	}

	/** Change the type hierarchy generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setTreeSummaryGenerator(TreeSummaryGenerator generator) {
		this.treeSummaryGenerator = generator;
	}

	/** Replies the type hierarchy generator.
	 *
	 * @return the generator.
	 */
	public TreeSummaryGenerator getTreeSummaryGenerator() {
		return this.treeSummaryGenerator;
	}

	/** Change the type hierarchy generator for a package.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setPackageTreeSummaryGenerator(PackageTreeSummaryGenerator generator) {
		this.packageTreeSummaryGenerator = generator;
	}

	/** Replies the type hierarchy generator for a package.
	 *
	 * @return the generator.
	 */
	public PackageTreeSummaryGenerator getPackageTreeSummaryGenerator() {
		return this.packageTreeSummaryGenerator;
	}

	/** Change the all-type summary generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setAllTypeSummaryGenerator(AllTypeSummaryGenerator generator) {
		this.allTypeSummaryGenerator = generator;
	}

	/** Replies the all-type summary generator.
	 *
	 * @return the generator.
	 */
	public AllTypeSummaryGenerator getAllTypeSummaryGenerator() {
		return this.allTypeSummaryGenerator;
	}

	/** Change the module summary generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setModuleSummaryGenerator(ModuleSummaryGenerator generator) {
		this.moduleSummaryGenerator = generator;
	}

	/** Replies the module summary generator.
	 *
	 * @return the generator.
	 */
	public ModuleSummaryGenerator getModuleSummaryGenerator() {
		return this.moduleSummaryGenerator;
	}

	/** Change the package summary generator.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setPackageSummaryGenerator(PackageSummaryGenerator generator) {
		this.packageSummaryGenerator = generator;
	}

	/** Replies the package summary generator.
	 *
	 * @return the generator.
	 */
	public PackageSummaryGenerator getPackageSummaryGenerator() {
		return this.packageSummaryGenerator;
	}

	/** Change the taglet factory.
	 *
	 * @param factory the factory
	 */
	@Inject
	public void setSarlTagletFactory(SarlTagletFactory factory) {
		this.sarlTagletFactory = factory;
	}
	
	/** Replies the taglet factory.
	 *
	 * @return the factory
	 */
	public SarlTagletFactory getSarlTagletFactory() {
		return this.sarlTagletFactory;
	}
	
	/** Change the injector for custom taglets.
	 *
	 * @param injector the member injector for custom taglets.
	 */
	@Inject
	public void setCustomTagletInjector(MembersInjector<CustomTaglet> injector) {
		this.customTagletInjector = injector;
	}
	
	/** Replies the injector for custom taglets.
	 *
	 * @return the member injector for custom taglets.
	 */
	public MembersInjector<CustomTaglet> getCustomTagletInjector() {
		return this.customTagletInjector;
	}

	/** Constructor.
	 *
	 * @param parent the parent doclet.
	 */
	public SarlHtmlDoclet(Doclet parent) {
		super(parent);
	}

	/** Change the CLI options for the doclet.
	 *
	 * @param options the options.
	 * @since 0.13
	 */
	@Inject
	public void setDocletOptions(DocletOptions options) {
		this.docletOptions = options;
	}
	
	/** Replies the CLI options.
	 *
	 * @return the options.
	 */
	public DocletOptions getDocletOptions() {
		return this.docletOptions;
	}


	/** Change the registered taglets.
	 *
	 * @param taglets the registered taglets.
	 */
	@Inject
	public void setRegisteredTaglets(@Named("registered-taglets") Set<Provider<? extends Taglet>> taglets) {
		this.registeredTaglets = taglets;
	}

	/** Replies the registered taglets.
	 *
	 * @return the registered taglets.
	 */
	public Set<Provider<? extends Taglet>> getRegisteredTaglets() {
		if (this.registeredTaglets == null) {
			return Collections.emptySet();
		}
		return Collections.unmodifiableSet(this.registeredTaglets);
	}

	/** Change the type hierarchy manager.
	 *
	 * @param manager the manager.
	 */
	@Inject
	public void setTypeHierarchy(TypeHierarchy manager) {
		this.typeHierarchy = manager;
	}

	/** Replies the type hierarchy manager.
	 *
	 * @return the manager.
	 */
	public TypeHierarchy getTypeHierarchy() {
		return this.typeHierarchy;
	}

	/** Change the type repository manager.
	 *
	 * @param manager the manager.
	 */
	@Inject
	public void setTypeRepository(TypeRepository manager) {
		this.typeRepository = manager;
	}

	/** Replies the type repository manager.
	 *
	 * @return the manager.
	 */
	public TypeRepository getTypeRepository() {
		return this.typeRepository;
	}

	/** Change the element filter.
	 *
	 * @param filter the element filter.
	 */
	@Inject
	public void setElementFilter(ElementFilter filter) {
		this.elementFilter = filter;
	}

	/** Replies the element filter.
	 *
	 * @return the element filter.
	 */
	public ElementFilter getElementFilter() {
		return this.elementFilter;
	}

	/** Change the type generators selector.
	 *
	 * @param selector the selector.
	 */
	@Inject
	public void setTypeDocumentationGeneratorSelector(TypeDocumentationGeneratorSelector selector) {
		this.typeGeneratorSelector = selector;
	}

	/** Replies the selector of the generators for the types.
	 *
	 * @return the selector.
	 */
	public TypeDocumentationGeneratorSelector getTypeDocumentationGeneratorSelector() {
		return this.typeGeneratorSelector;
	}

	/** Replies the output directory.
	 *
	 * @return the output directory.
	 */
	public Path getOutputDirectory() {
		return getDocletOptions().getOutputDirectory();
	}
	
	/** Change the output directory.
	 *
	 * @param output the output directory.
	 */
	public void setOutputDirectory(Path output) {
		getDocletOptions().setOutputDirectory(output);
	}

	@Override
	public Set<? extends Option> getSupportedOptions() {
		return Collections.unmodifiableSet(this.options);
	}

	/** Register the registered taglets in the task manager.
	 */
	protected void registerTagletsToTagletManager() {
		for (final Provider<? extends Taglet> tagletProvider : getRegisteredTaglets()) {
			final Taglet taglet = tagletProvider.get();
			getTagletManager().addTaglet(taglet, false);
		}
	}

	/** Register the custom tags in the task manager.
	 */
	protected void registerCustomTagsToTagletManager() {
		final CustomTagParser parser = getCustomTagParser();
		for (final String userTag : getDocletOptions().getUserTags()) {
			final CustomTag ctag = parser.parse(userTag, CustomTagLocation.EVERYWHERE, null);
			if (ctag != null) {
				final CustomTaglet ctaglet = new CustomTaglet(ctag);
				getCustomTagletInjector().injectMembers(ctaglet);
				getTagletManager().addTaglet(ctaglet, false);
			}
		}
	}

	@Override
	public void init(Locale locale, Reporter reporter) {
		super.init(locale, reporter);
		registerTagletsToTagletManager();
		registerCustomTagsToTagletManager();
	}
 
	@Override
	protected boolean generate(SarlDocletEnvironment environment) throws Exception {
		// Delete existing output folder.
		if (!getDocletOptions().isFakeOutput()) {
			final Path outputDir = getOutputDirectory();
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.SarlHtmlDoclet_4, outputDir.toString()));
			Files.cleanFolder(outputDir.toFile(), null, false, false);
		}
		// Build the list of types
		Iterable<? extends TypeElement> typeElements = getElementFilter().extractTypeElements(environment);
		// Build data structures
		buildTypeRepository(typeElements, environment);
		buildTypeHierarchy(typeElements, environment);
		// Generate the documentation
		generateTypeDocumentation(typeElements, environment);
		// Generate the summaries
		generateOverviewSummary(typeElements, environment);
		generateAllTypesSummary(typeElements, environment);
		generateModuleSummary(getTypeRepository().getModules(), environment);
		generatePackageSummary(getTypeRepository().getPackages(), environment);
		generateDeprecatedList(environment);
		generateTreeSummary(environment);
		generatePackageTreeSummary(getTypeRepository().getPackages(), environment);
		// Generate the indexes
		generateIndex(typeElements, environment);
		generateHtmlIndex(environment);
		generateOverviewFrame(environment);
		generateAllTypesFrame(environment);
		// Generate raw lists
		generateRawModuleList(environment);
		generateRawPackageList(environment);
		// Copy files
		copyResourceFiles(environment);
		return true;
	}

	/** Generate the index.
	 *
	 * @param typeElements is the list of the type elements for which the documentation should be generated.
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateIndex(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		getIndexGenerator().generate(getMapOfCssSheets().values(), 
				getMapOfJsScripts().values(), environment, opts, getReporter());
	}

	/** Generate the raw module list.
	 *
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateRawModuleList(SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		getRawModuleListGenerator().generate(environment, opts, getReporter());
	}

	/** Generate the raw package list.
	 *
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateRawPackageList(SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		getRawPackageListGenerator().generate(environment, opts, getReporter());
	}

	/** Generate the HTML main index.
	 *
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateHtmlIndex(SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		getHtmlIndexGenerator().generate(getMapOfCssSheets().values(), 
				getMapOfJsScripts().values(), environment, opts, getReporter());
	}

	/** Generate the overview frame.
	 *
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateOverviewFrame(SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		getOverviewFrameGenerator().generate(getMapOfCssSheets().values(), 
				getMapOfJsScripts().values(), environment, opts, getReporter());
	}

	/** Generate the all-types overview frame.
	 *
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateAllTypesFrame(SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		getAllTypesFrameGenerator().generate(getMapOfCssSheets().values(), 
				getMapOfJsScripts().values(), environment, opts, getReporter());
	}

	/** Generate the overview.
	 *
	 * @param typeElements is the list of the type elements for which the documentation should be generated.
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateOverviewSummary(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		getOverviewSummaryGenerator().generate(getMapOfCssSheets().values(), 
				getMapOfJsScripts().values(), environment, opts, getReporter());
	}

	/** Generate the deprecated list if it is enabled in the configuration.
	 *
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateDeprecatedList(SarlDocletEnvironment environment) throws Exception {
		if (getDocletOptions().isDeprecatedFeaturesEnabled()) {
			final DocletOptions opts = getDocletOptions();
			getDeprecatedListGenerator().generate(getMapOfCssSheets().values(), 
					getMapOfJsScripts().values(), environment, opts, getReporter());
		}
	}

	/** Generate the type hierarchy.
	 *
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateTreeSummary(SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		getTreeSummaryGenerator().generate(getMapOfCssSheets().values(), 
				getMapOfJsScripts().values(), environment, opts, getReporter());
	}

	/** Generate the type hierarchy for a package.
	 *
	 * @param packageElements is the list of the package elements for which the documentation should be generated.
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generatePackageTreeSummary(Iterable<? extends PackageElement> packageElements, SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		for (final PackageElement packageElement : packageElements) {
			getPackageTreeSummaryGenerator().generate(packageElement, getMapOfCssSheets().values(), 
				getMapOfJsScripts().values(), environment, opts, getReporter());
		}
	}

	/** Generate the list of all the types.
	 *
	 * @param typeElements is the list of the type elements for which the documentation should be generated.
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateAllTypesSummary(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		getAllTypeSummaryGenerator().generate(getMapOfCssSheets().values(), 
				getMapOfJsScripts().values(), environment, opts, getReporter());
	}

	/** Generate the summary of a module.
	 *
	 * @param moduleElements is the list of the module elements for which the documentation should be generated.
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateModuleSummary(Iterable<? extends ModuleElement> moduleElements, SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		for (final ModuleElement moduleElement : moduleElements) {
			getModuleSummaryGenerator().generate(moduleElement, getMapOfCssSheets().values(), 
					getMapOfJsScripts().values(), environment, opts, getReporter());
		}
	}

	/** Generate the summary of a package.
	 *
	 * @param packageElements is the list of the package elements for which the documentation should be generated.
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generatePackageSummary(Iterable<? extends PackageElement> packageElements, SarlDocletEnvironment environment) throws Exception {
		final DocletOptions opts = getDocletOptions();
		for (final PackageElement packageElement : packageElements) {
			getPackageSummaryGenerator().generate(packageElement, getMapOfCssSheets().values(), 
					getMapOfJsScripts().values(), environment, opts, getReporter());
		}
	}

	/** Replies the path to the resource located into the same folder as this class.
	 *
	 * @param basename the basename.
	 * @param resourceCallback the callback invoqued each time a resource is found.
	 * @throws Exception if some error occurs during the search of the resource.
	 */
	@SuppressWarnings("static-method")
	protected void findLocalResource(String basename, Consumer<URL> resourceCallback) throws Exception {
		assert resourceCallback != null;
		final URL url = Resources.getResource(SarlHtmlDoclet.class, basename);
		if (url != null) {
			resourceCallback.accept(url);
		} else {
			throw new FileNotFoundException(basename);
		}
	}

	private static String basename(URL url) {
		String bn = url.getPath();
		final int idx = bn.lastIndexOf('/');
		if (idx >= 0) {
			return bn.substring(idx + 1);
		}
		return bn;
	}

	/** Replies the CSS style sheets to be copied.
	 *
	 * @return The map of the source files (absolute) to the target files (relative to the output folder).
	 * @throws Exception if some error occurs during the search of the resource.
	 */
	protected Map<URL, Path> getMapOfCssSheets() throws Exception {
		if (this.cssResources == null) {
			this.cssResources = new LinkedHashMap<>();
			final Consumer<URL> consumer = it -> {
				final String basename = basename(it);
				final Path target = Path.of(CSS_FOLDER_NAME, basename);
				this.cssResources.put(it, target);
			};
			//
			for (final String cssResource : CssStyles.CSS_RESOURCES) {
				findLocalResource(cssResource, consumer);
			}
		}
		return this.cssResources;
	}

	/** Replies the Javascript resources to be copied.
	 *
	 * @return The map of the source files (absolute) to the target files (relative to the output folder).
	 * @throws Exception if some error occurs during the search of the resource.
	 */
	protected Map<URL, Path> getMapOfJsScripts() throws Exception {
		if (this.jsResources == null) {
			this.jsResources = new LinkedHashMap<>();
			final Consumer<URL> consumer = it -> {
				final String basename = basename(it);
				final Path target = Path.of(JS_FOLDER_NAME, basename);
				this.jsResources.put(it, target);
			};
			//
			for (final String cssResource : CssStyles.JS_RESOURCES) {
				findLocalResource(cssResource, consumer);
			}
		}
		return this.jsResources;
	}

	/** Copy all the resource files that are needed for the generated documentation.
	 *
	 * @param environment the generation environment.
	 * @throws Exception if some error occurs during the copy.
	 */
	protected void copyResourceFiles(SarlDocletEnvironment environment) throws Exception {
		final Path outputDir = getOutputDirectory();
		getReporter().print(Kind.NOTE, MessageFormat.format(Messages.SarlHtmlDoclet_10, outputDir.toString()));
		//
		final Iterable<Entry<URL, Path>> listOfResources = Iterables.concat(getMapOfCssSheets().entrySet(), getMapOfJsScripts().entrySet());
		for (final Entry<URL, Path> copying : listOfResources) {
			final URL source = copying.getKey();
			final Path target = copying.getValue();
			final Path rtarget = outputDir.resolve(target);
			try {
				java.nio.file.Files.createDirectories(rtarget.getParent());
				try (final InputStream is = source.openStream()) {
					java.nio.file.Files.copy(is, rtarget, StandardCopyOption.REPLACE_EXISTING);
				}
			} catch (Throwable ex) {
				getReporter().print(Kind.ERROR, MessageFormat.format(Messages.SarlHtmlDoclet_11, source.toString(), ex.getLocalizedMessage()));
			}
		}
		// TODO: Fixing issue in maven-javadoc-plugin
		final Path optionsPath = getDocletOptions().getOutputDirectory().resolve("options"); //$NON-NLS-1$
		if (!java.nio.file.Files.exists(optionsPath)) {
			if (!java.nio.file.Files.exists(optionsPath.getParent())) {
				java.nio.file.Files.createDirectories(optionsPath.getParent());
			}
			try (BufferedWriter writer = java.nio.file.Files.newBufferedWriter(optionsPath)) {
				//
			}
		}
		// TODO: Fixing issue in maven-javadoc-plugin
		final Path packagelistPath = getDocletOptions().getOutputDirectory().resolve("package-list"); //$NON-NLS-1$
		final Path packagesPath = getDocletOptions().getOutputDirectory().resolve("packages"); //$NON-NLS-1$
		java.nio.file.Files.copy(packagelistPath, packagesPath);
	}

	/** Build the global type hierarchy.
	 *
	 * @param typeElements is the list of the type elements for which the documentation should be generated.
	 * @param environment the generation environment.
	 */
	protected void buildTypeHierarchy(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment) {
		getReporter().print(Kind.NOTE, Messages.SarlHtmlDoclet_7);
		getTypeHierarchy().buildTree(typeElements, environment);
	}

	/** Build the global type repository.
	 *
	 * @param typeElements is the list of the type elements for which the documentation should be generated.
	 * @param environment the generation environment.
	 */
	protected void buildTypeRepository(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment) {
		getReporter().print(Kind.NOTE, Messages.SarlHtmlDoclet_21);
		getTypeRepository().buildRepository(typeElements, environment);
	}

	/** Generate the documentation for the types.
	 *
	 * @param typeElements is the list of the type elements for which the documentation should be generated.
	 * @param environment the generation environment.
	 * @throws Exception if an error occurred during the generation.
	 */
	protected void generateTypeDocumentation(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment) throws Exception {
		final TypeDocumentationGeneratorSelector selector = getTypeDocumentationGeneratorSelector();
		final DocletOptions opts = getDocletOptions();
		for (final TypeElement element : typeElements) {
			final TypeDocumentationGenerator generator = selector.getTypeGeneratorFor(element, environment);
			if (generator == null) {
				throw new Exception(MessageFormat.format(Messages.SarlHtmlDoclet_2, element.getQualifiedName().toString()));
			}
			generator.generate(element, getMapOfCssSheets().values(), 
					getMapOfJsScripts().values(), environment, opts, getReporter());
		}
	}

	/** Abstract definition of a CLI option.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	public static abstract class Option implements Doclet.Option, Comparable<Option> {

		private final String[] names;

		private final String parameter;

		private final String description;

		private final int parameterCount;

		/** Constructor.
		 *
		 * @param name the name of the option.
		 * @param description the description of the resource.
		 * @param parameter is the description of the parameter.
		 */
		public Option(String name, String description, String parameter) {
			this(name, description, parameter, parameter != null ? 1 : 0);
		}

		/** Constructor.
		 *
		 * @param name the name of the option.
		 * @param description the description of the resource.
		 * @param parameter is the description of the parameter.
		 * @param parameterCount the number of parameters.
		 */
		public Option(String name, String description, String parameter, int parameterCount) {
			this.names = name.trim().split("\\s+"); //$NON-NLS-1$
			this.description = description;
			this.parameter = parameter;
			this.parameterCount = parameterCount;
		}

		/** Constructor.
		 *
		 * @param name the name of the option.
		 * @param description the description of the resource.
		 */
		public Option(String name, String description) {
			this(name, description, null);
		}

		@Override
		public String getDescription() {
			return this.description;
		}

		@Override
		public Option.Kind getKind() {
			return Doclet.Option.Kind.STANDARD;
		}

		@Override
		public List<String> getNames() {
			return Arrays.asList(this.names);
		}

		@Override
		public String getParameters() {
			return this.parameter;
		}

		@Override
		public String toString() {
			return Arrays.toString(this.names);
		}

		@Override
		public int getArgumentCount() {
			return this.parameterCount;
		}

		@Override
		public int compareTo(Option that) {
			return this.getNames().get(0).compareTo(that.getNames().get(0));
		}

	}

}
