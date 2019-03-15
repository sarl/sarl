/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendFunction;

import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Action</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlAction#getFiredEvents <em>Fired Events</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlAction()
 * @model
 * @generated
 */
public interface SarlAction extends XtendFunction
{
	/**
	 * Returns the value of the '<em><b>Fired Events</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.xtext.common.types.JvmTypeReference}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Fired Events</em>' containment reference list.
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlAction_FiredEvents()
	 * @model containment="true"
	 * @generated
	 */
	EList<JvmTypeReference> getFiredEvents();

} // SarlAction
