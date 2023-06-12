package org.arakhne.afc.bootique.variables;

import io.bootique.BQCoreModuleExtender;
import io.bootique.di.Binder;

// FIXME: Move to Arakhne
public class VariableDecls {

	private final BQCoreModuleExtender extender;

	/** Constructor.
	 *
	 * @param extender the Bootique extender.
	 */
	protected VariableDecls(BQCoreModuleExtender extender) {
		this.extender = extender;
	}

	/** Create an extended from the given binder.
	 *
	 * @param binder the injection binder.
	 * @return the variable declarator.
	 */
	public static VariableDecls extend(Binder binder) {
		return new VariableDecls(io.bootique.BQCoreModule.extend(binder));
	}

	/** Declare an environment variable which is linked to the given Bootique variable, and has its name defined
	 * from the name of the Bootique variable.
	 *
	 * @param bootiqueVariable the name of the bootique variable.
	 * @return the Bootique extender.
	 */
	public VariableDecls declareVar(String bootiqueVariable) {
		this.extender.declareVar(bootiqueVariable, VariableNames.toEnvironmentVariableName(bootiqueVariable));
		return this;
	}

}
