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

package io.sarl.lang.ecoregenerator.fragments;

import org.apache.log4j.Logger;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.xpand2.XpandExecutionContext;
import org.eclipse.xtext.AbstractMetamodelDeclaration;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.generator.DefaultGeneratorFragment;
import org.eclipse.xtext.generator.IGeneratorFragment;
import org.eclipse.xtext.util.Strings;

/**
 * A {@link IGeneratorFragment} that update the definition of the Ecore models with
 * specific SARL functions that could not be specified with fake rules in the grammar.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLEcoreUpdaterFragment extends DefaultGeneratorFragment {

	private static final Logger log = Logger.getLogger(SARLEcoreUpdaterFragment.class);

	@Override
	public void generate(Grammar grammar, XpandExecutionContext ctx) {
		log.info("Updating SARL Ecore Package with additional elements."); //$NON-NLS-1$
		
		if (grammar == null) {
			throw new RuntimeException("No grammar defined"); //$NON-NLS-1$
		}
		
		for (AbstractMetamodelDeclaration declaration : grammar.getMetamodelDeclarations()) {
			EPackage epackage = declaration.getEPackage();
			String name = epackage.getName();
			if (Strings.isEmpty(name)) {
				name = declaration.getAlias();
			}
			if ("sarl".equals(name)) { //$NON-NLS-1$
				updateEcorePackage(epackage);
				return;
			}
		}

		throw new RuntimeException("No SARL Ecore element were defined"); //$NON-NLS-1$
	}

	/** Update the Ecore elements of the package.
	 *
	 * @param sarlPackage - the package of SARL elements.
	 */
	protected static void updateEcorePackage(EPackage sarlPackage) {
		addMethodInClass(sarlPackage,
				"SarlAgent", //$NON-NLS-1$
				"isAbstract", //$NON-NLS-1$
				EcorePackage.eINSTANCE.getEBoolean());
		addMethodInClass(sarlPackage,
				"SarlBehavior", //$NON-NLS-1$
				"isAbstract", //$NON-NLS-1$
				EcorePackage.eINSTANCE.getEBoolean());
		addMethodInClass(sarlPackage,
				"SarlEvent", //$NON-NLS-1$
				"isAbstract", //$NON-NLS-1$
				EcorePackage.eINSTANCE.getEBoolean());
		addMethodInClass(sarlPackage,
				"SarlSkill", //$NON-NLS-1$
				"isAbstract", //$NON-NLS-1$
				EcorePackage.eINSTANCE.getEBoolean());
	}
	
	/** Add a method in the definition of the givne class.
	 *
	 * @param epackage - the containing package.
	 * @param classname - the name of the class to upgrade.
	 * @param functionName - the name of the new function.
	 * @param returnType - the return type.
	 */
	protected static void addMethodInClass(EPackage epackage, String classname, String functionName, EClassifier returnType) {
		log.info("\tadding " + functionName + " into " + classname); //$NON-NLS-1$ //$NON-NLS-2$
		EClassifier eclassifier = epackage.getEClassifier(classname);
		if (eclassifier == null || !(eclassifier instanceof EClass)) {
			throw new RuntimeException("No a class with name: " + classname); //$NON-NLS-1$
		}
		EClass eclass = (EClass) eclassifier;
		EOperation eoperation = EcoreFactory.eINSTANCE.createEOperation();
		eoperation.setName(functionName);
		if (returnType != null) {
			eoperation.setEType(returnType);
		}
		eclass.getEOperations().add(eoperation);
	}
	
}

