/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendMember;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Protocol Message</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocolMessage#getFrom <em>From</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocolMessage#getTo <em>To</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocolMessage#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocolMessage#getParameters <em>Parameters</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlProtocolMessage#getRawTarget <em>Raw Target</em>}</li>
 * </ul>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolMessage()
 * @model
 * @generated
 */
public interface SarlProtocolMessage extends XtendMember
{
	/**
	 * Returns the value of the '<em><b>From</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>From</em>' attribute.
	 * @see #setFrom(String)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolMessage_From()
	 * @model
	 * @generated
	 */
	String getFrom();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlProtocolMessage#getFrom <em>From</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>From</em>' attribute.
	 * @see #getFrom()
	 * @generated
	 */
	void setFrom(String value);

	/**
	 * Returns the value of the '<em><b>To</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>To</em>' attribute.
	 * @see #setTo(String)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolMessage_To()
	 * @model
	 * @generated
	 */
	String getTo();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlProtocolMessage#getTo <em>To</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>To</em>' attribute.
	 * @see #getTo()
	 * @generated
	 */
	void setTo(String value);

	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #setName(String)
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolMessage_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link io.sarl.lang.sarl.SarlProtocolMessage#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Returns the value of the '<em><b>Parameters</b></em>' containment reference list.
	 * The list contents are of type {@link io.sarl.lang.sarl.SarlProtocolParameter}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Parameters</em>' containment reference list.
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolMessage_Parameters()
	 * @model containment="true"
	 * @generated
	 */
	EList<SarlProtocolParameter> getParameters();

	/**
	 * Returns the value of the '<em><b>Raw Target</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Raw Target</em>' attribute list.
	 * @see io.sarl.lang.sarl.SarlPackage#getSarlProtocolMessage_RawTarget()
	 * @model unique="false"
	 * @generated
	 */
	EList<String> getRawTarget();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation" unique="false"
	 * @generated
	 */
	EList<String> getModifiers();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 * @generated
	 */
	boolean isOutTargetRole();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 * @generated
	 */
	boolean isInTargetRole();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 * @generated
	 */
	String getIdentifier();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model kind="operation"
	 * @generated
	 */
	SarlProtocol getProtocol();

} // SarlProtocolMessage
