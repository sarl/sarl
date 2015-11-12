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

package io.sarl.lang.ecoregenerator.mwe2;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EParameter;
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

	private static final Logger LOG = Logger.getLogger(SARLEcoreUpdaterFragment.class);

	private final List<MethodUpdater> methodUpdaters = new ArrayList<>();

	/** Replies the Ecore classifier for the given type.
	 *
	 * <p>It must be accessible with {@link EcorePackage#eINSTANCE}.
	 *
	 * @param name the name.
	 * @return the classifier.
	 */
	protected static EClassifier findType(String name) {
		if (!Strings.isEmpty(name)) {
			String upperFirst =
					name.substring(0, 1).toUpperCase()
					+ name.substring(1);
			try {
				Method method = EcorePackage.class.getMethod("getE" + upperFirst); //$NON-NLS-1$
				return (EClassifier) method.invoke(EcorePackage.eINSTANCE);
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
		return null;
	}

	/** Add a method in the definition of the givne class.
	 *
	 * @param epackage - the containing package.
	 * @param classname - the name of the class to upgrade.
	 * @param functionName - the name of the new function.
	 * @param returnType - the return type.
	 * @param parameters - the formal parameters.
	 */
	protected static void addMethodInClass(EPackage epackage, String classname, String functionName,
			EClassifier returnType, List<FormalParameter> parameters) {
		LOG.info("\tadding " + functionName + " into " + classname); //$NON-NLS-1$ //$NON-NLS-2$
		EClassifier eclassifier = epackage.getEClassifier(classname);
		if (eclassifier == null || !(eclassifier instanceof EClass)) {
			throw new RuntimeException("No a class with name: " + classname); //$NON-NLS-1$
		}
		EOperation eoperation = EcoreFactory.eINSTANCE.createEOperation();
		eoperation.setName(functionName);
		if (returnType != null) {
			eoperation.setEType(returnType);
		}
		for (FormalParameter parameter : parameters) {
			EParameter eparameter = EcoreFactory.eINSTANCE.createEParameter();
			eparameter.setName(parameter.getName());
			eparameter.setEType(parameter.getType());
			eoperation.getEParameters().add(eparameter);
		}
		((EClass) eclassifier).getEOperations().add(eoperation);
	}

	/** Register an updater for adding a method into an Ecore.
	 *
	 * @param updater - the updater.
	 */
	public void addMethod(MethodUpdater updater) {
		if (updater != null) {
			this.methodUpdaters.add(updater);
		}
	}

	@Override
	public void generate(Grammar grammar, XpandExecutionContext ctx) {
		LOG.info("Updating SARL Ecore Package with additional elements."); //$NON-NLS-1$

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
	protected void updateEcorePackage(EPackage sarlPackage) {
		for (MethodUpdater updater : this.methodUpdaters) {
			addMethodInClass(sarlPackage,
					updater.getContainerName(),
					updater.getMethodName(),
					updater.getReturnType(),
					updater.getParameters());
		}
	}

	/** Describe the addition of a method in a Ecore container.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class MethodUpdater {

		private String className;

		private String methodName;

		private String returnType;

		private final List<FormalParameter> parameters = new ArrayList<>();

		/** Set the name of the container.
		 *
		 * @param classname - the name of the container in the EPackage.
		 */
		public void setContainerName(String classname) {
			this.className = classname;
		}

		/** Replies the name of the container.
		 *
		 * @return the name of the container in the EPackage.
		 */
		public String getContainerName() {
			if (Strings.isEmpty(this.className)) {
				throw new RuntimeException("no classname specified."); //$NON-NLS-1$
			}
			return this.className;
		}

		/** Set the name of the method to add.
		 *
		 * @param name - the name of the method.
		 */
		public void setMethodName(String name) {
			this.methodName = name;
		}

		/** Replies the name of the method to add.
		 *
		 * @return the name of the method.
		 */
		public String getMethodName() {
			if (Strings.isEmpty(this.className)) {
				throw new RuntimeException("no method name specified."); //$NON-NLS-1$
			}
			return this.methodName;
		}

		/** Set the return type.
		 *
		 * <p>It must be accessible with {@link EcorePackage#eINSTANCE}.
		 *
		 * @param type - the name of the type.
		 */
		public void setReturnType(String type) {
			this.returnType = type;
		}

		/** Replies the return type.
		 *
		 * <p>It must be accessible with {@link EcorePackage#eINSTANCE}.
		 *
		 * @return the return type.
		 */
		public EClassifier getReturnType() {
			return findType(this.returnType);
		}

		/** Add a formal parameter.
		 *
		 * @param parameter - the parameter.
		 */
		public void addParameter(FormalParameter parameter) {
			if (parameter != null) {
				this.parameters.add(parameter);
			}
		}

		/** Replies the formal parameters.
		 *
		 * @return the formal parameters.
		 */
		public List<FormalParameter> getParameters() {
			return this.parameters;
		}

	}

	/** Describe the addition of a formal parameter.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class FormalParameter {

		private String name;

		private String type;

		/** Set the name of the formal parameter.
		 *
		 * @param name - the name of the formal parameter.
		 */
		public void setName(String name) {
			this.name = name;
		}

		/** Replies the name of the formal parameter.
		 *
		 * @return the name of the formal parameter.
		 */
		public String getName() {
			if (Strings.isEmpty(this.name)) {
				throw new RuntimeException("no parameter name specified."); //$NON-NLS-1$
			}
			return this.name;
		}

		/** Set the type.
		 *
		 * <p>It must be accessible with {@link EcorePackage#eINSTANCE}.
		 *
		 * @param type - the name of the formal parameter.
		 */
		public void setType(String type) {
			this.type = type;
		}

		/** Replies the type.
		 *
		 * <p>It must be accessible with {@link EcorePackage#eINSTANCE}.
		 *
		 * @return the return type.
		 */
		public EClassifier getType() {
			return findType(this.type);
		}

	}

}

