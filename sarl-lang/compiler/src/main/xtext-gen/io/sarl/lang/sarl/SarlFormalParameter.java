/**
 */
package io.sarl.lang.sarl;

import org.eclipse.xtend.core.xtend.XtendParameter;

import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Formal Parameter</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlFormalParameter#getDefaultValue <em>Default Value</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlFormalParameter()
 * @model
 * @generated
 */
public interface SarlFormalParameter extends XtendParameter
{
	/**
	 * Returns the value of the '<em><b>Default Value</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Default Value</em>' containment reference.
	 * @see #setDefaultValue(XExpression)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlFormalParameter_DefaultValue()
	 * @model containment="true"
	 * @generated
	 */
	XExpression getDefaultValue();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlFormalParameter#getDefaultValue <em>Default Value</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Default Value</em>' containment reference.
	 * @see #getDefaultValue()
	 * @generated
	 */
	void setDefaultValue(XExpression value);

} // SarlFormalParameter
