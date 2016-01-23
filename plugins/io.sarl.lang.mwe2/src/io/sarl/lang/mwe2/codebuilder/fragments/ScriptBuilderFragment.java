/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.inject.Inject;
import javax.inject.Provider;

import com.google.inject.Injector;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;
import org.eclipse.xtext.xtext.generator.model.XtendFileAccess;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XImportSection;
import org.eclipse.xtext.xtype.XtypeFactory;

/** Generator of the script builder types.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ScriptBuilderFragment extends AbstractSubCodeBuilderFragment {

	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	/** Replies the custom implementation for the script builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getScriptBuilderImplCustom() {
		return new TypeReference(getBuilderPackage() + ".ScriptBuilderImplCustom"); //$NON-NLS-1$
	}

	/** Replies the implementation for the builder of scripts.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getScriptBuilderImpl() {
		return new TypeReference(getBuilderPackage() + ".ScriptBuilderImpl"); //$NON-NLS-1$
	}

	@Override
	protected Collection<AbstractSubCodeBuilderFragment> initializeSubGenerators(Injector injector) {
		return Arrays.asList(
				injector.getInstance(ExpressionBuilderFragment.class),
				injector.getInstance(BlockExpressionBuilderFragment.class),
				injector.getInstance(FormalParameterBuilderFragment.class),
				injector.getInstance(TopElementBuilderFragment.class));
	}

	@Override
	public void generate() {
		generateIScriptBuilder();
		generateScriptBuilderImpl();
		generateBuilderFactoryContributions();
		super.generate();
	}

	/** Generate the contributions for the BuildFactory.
	 */
	protected void generateBuilderFactoryContributions() {
		this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\t/** Create the factory for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resourceSet the resource set in which the script is created."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" createScript(String packageName, "); //$NON-NLS-1$
				it.append(ResourceSet.class);
				it.append(" resourceSet) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn createScript(packageName, createResource(resourceSet));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create the factory for a " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * @param packageName the name of the package of the script."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resource the resource in which the script is created."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" createScript(String packageName, "); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" builder = getProvider("); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(".class).get();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tbuilder.eInit(resource, packageName);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn builder;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		});
	}

	@Override
	public void generateXtendStubs() {
		super.generateXtendStubs();
		TypeReference stub = getScriptBuilderImplCustom();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** User-defined builder of the " + getLanguageName() + " scripts."); //$NON-NLS-1$//$NON-NLS-2$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("class "); //$NON-NLS-1$
				it.append(stub);
				it.append(" extends "); //$NON-NLS-1$
				it.append(getScriptBuilderImpl());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLine();
			}

		};
		XtendFileAccess xtendFile = getFileAccessFactory().createXtendFile(stub, content);
		IFileSystemAccess2 fileSystem = getSrc();
		if (!fileSystem.isFile(xtendFile.getPath())) {
			xtendFile.writeTo(fileSystem);
		}
	}

	@Override
	public void generateJavaStubs() {
		super.generateJavaStubs();
		final TypeReference stub = getScriptBuilderImplCustom();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** User-defined builder of the " + getLanguageName() + " scripts."); //$NON-NLS-1$//$NON-NLS-2$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(stub);
				it.append(" extends "); //$NON-NLS-1$
				it.append(getScriptBuilderImpl());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLine();
			}

		};
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(stub, content);
		IFileSystemAccess2 fileSystem = getSrc();
		if (!fileSystem.isFile(javaFile.getPath())) {
			javaFile.writeTo(fileSystem);
		}
	}

	@Override
	public void generateBindings(BindingFactory factory) {
		super.generateBindings(factory);
		IFileSystemAccess2 fileSystem = getSrc();
		TypeReference type;
		if ((fileSystem.isFile(getScriptBuilderImplCustom().getJavaPath()))
				|| (fileSystem.isFile(getScriptBuilderImplCustom().getXtendPath()))) {
			type = getScriptBuilderImplCustom();
		} else {
			type = getScriptBuilderImpl();
		}
		factory.addfinalTypeToType(getScriptBuilderInterface(), type);
	}

	/** Extract a top element from the grammar.
	 *
	 * @param rule the rule to extract.
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @return the top element.
	 */
	protected StringConcatenationClient generateTopElement(AbstractRule rule, boolean forInterface) {
		final String topElementName = Strings.toFirstUpper(rule.getName());
		final TypeReference builderType = getElementBuilderInterface(topElementName);
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				if (!forInterface) {
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(Provider.class);
					it.append("<"); //$NON-NLS-1$
					it.append(builderType);
					it.append("> "); //$NON-NLS-1$
					it.append(Strings.toFirstLower(topElementName));
					it.append("Provider;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Create " + getAorAnArticle(topElementName) //$NON-NLS-1$
				+ " "+ topElementName + " builder.");  //$NON-NLS-1$//$NON-NLS-2$
				it.newLine();
				it.append("\t * @param name - the name of the " + topElementName + "."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * @return the builder."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(builderType);
				it.append(" add"); //$NON-NLS-1$
				it.append(topElementName);
				it.append("(String name)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(builderType);
					it.append(" builder = this."); //$NON-NLS-1$
					it.append(Strings.toFirstLower(topElementName));
					it.append("Provider.get();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tbuilder.eInit(getScript(), name);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn builder;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
				}
				it.newLine();
			}
		};
	}

	/** Extract top elements from the grammar.
	 *
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @return the top elements.
	 */
	protected List<StringConcatenationClient> generateTopElements(boolean forInterface) {
		Set<String> topElementNames = new TreeSet<>();
		Grammar grammar = getGrammar();
		List<StringConcatenationClient> topElements = new ArrayList<>();
		AbstractRule rule = GrammarUtil.findRuleForName(grammar, getCodeBuilderConfig().getTopElementRuleName());
		if (rule != null) {
			for (RuleCall ruleCall : GrammarUtil.containedRuleCalls(rule)) {
				topElements.add(generateTopElement(ruleCall.getRule(), forInterface));
				topElementNames.add(ruleCall.getRule().getName());
			}
		}
		return topElements;
	}

	/** Generate the script builder interface.
	 */
	protected void generateIScriptBuilder() {
		List<StringConcatenationClient> topElements = generateTopElements(true);
		final TypeReference builder = getScriptBuilderInterface();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of " + getLanguageName() + " scripts."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append(" *"); //$NON-NLS-1$
				it.newLine();
				it.append(" * <p>This builder is provided for helping to create " //$NON-NLS-1$
						+ getLanguageName()
						+ " Ecore elements."); //$NON-NLS-1$
				it.newLine();
				it.append(" *"); //$NON-NLS-1$
				it.newLine();
				it.append(" * <p>Do not forget to invoke {@link #finalizeScript()} for creating imports, etc."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public interface "); //$NON-NLS-1$
				it.append(builder.getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateFieldsAndMethods(true));
				for (StringConcatenationClient element : topElements) {
					it.append(element);
				}
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the script builder default implementation.
	 */
	protected void generateScriptBuilderImpl() {
		List<StringConcatenationClient> topElements = generateTopElements(false);
		final TypeReference script = getScriptBuilderImpl();
		final TypeReference scriptInterface = getScriptBuilderInterface();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(script.getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(getAbstractBuilderImpl());
				it.append(" implements "); //$NON-NLS-1$
				it.append(scriptInterface);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateFieldsAndMethods(false));
				for (StringConcatenationClient element : topElements) {
					it.append(element);
				}
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
			}

		};
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(script, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the fields and the methods.
	 *
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @return the fields and methods.
	 */
	protected StringConcatenationClient generateFieldsAndMethods(boolean forInterface) {
		TypeReference scriptInterface = getLanguageScriptInterface();
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				// Fields
				if (!forInterface) {
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(scriptInterface);
					it.append(" script;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Create the internal " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource, String packageName)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tif (this.script == null) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tthis.script = "); //$NON-NLS-1$
					it.append(getXFactoryFor(scriptInterface));
					it.append(".eINSTANCE.create"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(scriptInterface.getSimpleName()));
					it.append("();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t"); //$NON-NLS-1$
					it.append(EList.class);
					it.append("<"); //$NON-NLS-1$
					it.append(EObject.class);
					it.append("> content = resource.getContents();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tif (!content.isEmpty()) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tcontent.clear();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tcontent.add(this.script);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tif (!"); //$NON-NLS-1$
					it.append(Strings.class);
					it.append(".isEmpty(packageName)) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tscript.setPackage(packageName);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the " + getLanguageName() + " script."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(scriptInterface);
				it.append(" getScript()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn this.script;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource to which the script is attached."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(Resource.class);
				it.append(" eResource()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn getScript().eResource();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Finalize the script."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * <p>The finalization includes: <ul>"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * <li>The import section is created.</li>"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * </ul>"); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void finalizeScript()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(ImportManager.class);
					it.append(" concreteImports = new "); //$NON-NLS-1$
					it.append(ImportManager.class);
					it.append("();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(XImportSection.class);
					it.append(" importSection = getScript().getImportSection();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tif (importSection != null) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tfor ("); //$NON-NLS-1$
					it.append(XImportDeclaration.class);
					it.append(" decl : importSection.getImportDeclarations()) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tconcreteImports.addImportFor(decl.getImportedType());"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tfor (String importName : getImportManager().getImports()) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t"); //$NON-NLS-1$
					it.append(JvmType.class);
					it.append(" type = getTypeReferences().findDeclaredType(importName, getScript());"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tif (type instanceof "); //$NON-NLS-1$
					it.append(JvmDeclaredType.class);
					it.newLine();
					it.append("\t\t\t\t\t&& concreteImports.addImportFor(type)) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t"); //$NON-NLS-1$
					it.append(XImportDeclaration.class);
					it.append(" declaration = "); //$NON-NLS-1$
					it.append(XtypeFactory.class);
					it.append(".eINSTANCE.createXImportDeclaration();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tdeclaration.setImportedType(("); //$NON-NLS-1$
					it.append(JvmDeclaredType.class);
					it.append(") type);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tif (importSection == null) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t\timportSection = "); //$NON-NLS-1$
					it.append(XtypeFactory.class);
					it.append(".eINSTANCE.createXImportSection();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t\tgetScript().setImportSection(importSection);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\timportSection.getImportDeclarations().add(declaration);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
	}

}
