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

package io.sarl.lang.pythongenerator.validator;

import java.text.MessageFormat;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.featurecalls.IdentifiableSimpleNameProvider;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure3;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XtypePackage;
import org.osgi.framework.Bundle;

import io.sarl.lang.extralanguage.compiler.AbstractExtraLanguageGenerator.ExtraLanguageSupportModule;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageConversionInitializer;
import io.sarl.lang.extralanguage.validator.AbstractExtraLanguageValidator;
import io.sarl.lang.pythongenerator.PyGeneratorPlugin;
import io.sarl.lang.pythongenerator.configuration.IPyGeneratorConfigurationProvider;
import io.sarl.lang.pythongenerator.generator.PyInitializers;
import io.sarl.lang.pythongenerator.generator.PyKeywordProvider;

/** The validator from SARL to the Python target language.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version pythongenerator 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid pythongenerator
 * @since 0.6
 */
@Singleton
public class PyValidator extends AbstractExtraLanguageValidator {

	/** Error handler for the type conversions.
	 */
	private final Procedure3<? super EObject, ? super JvmType, ? super String> typeErrorHandler1 = (source, invalidType, name) -> {
		error(MessageFormat.format(Messages.PyValidator_0, name), source,
				XtypePackage.eINSTANCE.getXImportDeclaration_ImportedType());
	};

	/** Error handler for the type conversions.
	 */
	private final Procedure3<? super EObject, ? super JvmType, ? super String> typeErrorHandler2 = (source, invalidType, name) -> {
		error(MessageFormat.format(Messages.PyValidator_0, name), source,
				XbasePackage.eINSTANCE.getXAbstractFeatureCall_Feature());
	};

	/** Error handler for the feature conversions.
	 */
	private final Function2<? super EObject, ? super JvmIdentifiableElement, ? extends Boolean> featureErrorHandler = (source, element) -> {
		final String message;
		if (element instanceof JvmConstructor) {
			message = MessageFormat.format(Messages.PyValidator_1, this.simpleNameProvider.getSimpleName(element));
		} else if (element instanceof JvmField) {
			message = MessageFormat.format(Messages.PyValidator_2, this.simpleNameProvider.getSimpleName(element));
		} else if (element instanceof JvmOperation) {
			message = MessageFormat.format(Messages.PyValidator_3, this.simpleNameProvider.getSimpleName(element));
		} else {
			// This type of JVM element is not supposed to be converted
			return Boolean.FALSE;
		}
		error(message, source, XbasePackage.eINSTANCE.getXAbstractFeatureCall_Feature());
		return Boolean.TRUE;
	};

	@Inject
	private IdentifiableSimpleNameProvider simpleNameProvider;

	/** Validator's logger.
	 */
	@Inject
	private Logger log;

	private IPyGeneratorConfigurationProvider configuration;

	/** Constructor.
	 *
	 * @param keywordProvider the provider of Python keywords.
	 */
	@Inject
	public PyValidator(PyKeywordProvider keywordProvider) {
		super(keywordProvider);
	}

	/** Replies the support module for the initialization stage.
	 *
	 * @return the module, never {@code null}.
	 * @since 0.14
	 */
	protected ExtraLanguageSupportModule getSupportModule() {
		return new ExtraLanguageSupportModule() {

			@Override
			public Bundle getBundle() {
				return PyGeneratorPlugin.getBundle();
			}

			@Override
			public void logException(Throwable exception) {
				if (exception != null) {
					PyValidator.this.log.log(Level.SEVERE, exception.getLocalizedMessage(), exception);
				}
			}
			
		};
	}

	/** Change the provider of the generator's configuration.
	 *
	 * @param provider the provider.
	 */
	@Inject
	public void setPyGeneratorConfigurationProvider(IPyGeneratorConfigurationProvider provider) {
		this.configuration = provider;
	}

	@Override
	protected void initializeContext(Context validatorContext) {
		final var resource = validatorContext.getCurrentObject().eResource();
		final var config = this.configuration.get(resource, true);
		final var converter = getTypeConverter();
		converter.setImplicitJvmTypes(config.isImplicitJvmTypes());
	}

	@Override
	protected IExtraLanguageConversionInitializer getTypeConverterInitializer() {
		return PyInitializers.getTypeConverterInitializer(getSupportModule());
	}

	@Override
	protected IExtraLanguageConversionInitializer getFeatureConverterInitializer() {
		return PyInitializers.getFeatureNameConverterInitializer(getSupportModule());
	}

	@Override
	protected String getErrorMessageFormat() {
		return Messages.PyValidator_4;
	}

	/** Check that import mapping are known.
	 *
	 * @param importDeclaration the declaration.
	 */
	@Check
	public void checkImportsMapping(XImportDeclaration importDeclaration) {
		final var type = importDeclaration.getImportedType();
		doTypeMappingCheck(importDeclaration, type, this.typeErrorHandler1);
	}

	/** Check that member feature calls have a conversion mapping.
	 *
	 * @param featureCall the feature call.
	 */
	@Check
	public void checkMemberFeatureCallMapping(XMemberFeatureCall featureCall) {
		doCheckMemberFeatureCallMapping(featureCall, this.typeErrorHandler2, this.featureErrorHandler);
	}

	/** Check that member feature calls have a conversion mapping.
	 *
	 * @param featureCall the feature call.
	 */
	@Check
	public void checkMemberFeatureCallMapping(XFeatureCall featureCall) {
		doCheckMemberFeatureCallMapping(featureCall, this.typeErrorHandler2, this.featureErrorHandler);
	}

}
