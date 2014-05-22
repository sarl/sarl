/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Action Signature</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.ActionSignature#getName <em>Name</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.ActionSignature#getType <em>Type</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.ActionSignature#getFiredEvents <em>Fired Events</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getActionSignature()
 * @model
 * @generated
 */
public interface ActionSignature extends ParameterizedFeature
{
  /**
   * Returns the value of the '<em><b>Name</b></em>' attribute.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Name</em>' attribute isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Name</em>' attribute.
   * @see #setName(String)
   * @see io.sarl.lang.sarl.SarlPackage#getActionSignature_Name()
   * @model
   * @generated
   */
  String getName();

  /**
   * Sets the value of the '{@link io.sarl.lang.sarl.ActionSignature#getName <em>Name</em>}' attribute.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Name</em>' attribute.
   * @see #getName()
   * @generated
   */
  void setName(String value);

  /**
   * Returns the value of the '<em><b>Type</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Type</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Type</em>' containment reference.
   * @see #setType(JvmTypeReference)
   * @see io.sarl.lang.sarl.SarlPackage#getActionSignature_Type()
   * @model containment="true"
   * @generated
   */
  JvmTypeReference getType();

  /**
   * Sets the value of the '{@link io.sarl.lang.sarl.ActionSignature#getType <em>Type</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Type</em>' containment reference.
   * @see #getType()
   * @generated
   */
  void setType(JvmTypeReference value);

  /**
   * Returns the value of the '<em><b>Fired Events</b></em>' reference list.
   * The list contents are of type {@link io.sarl.lang.sarl.Event}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Fired Events</em>' reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Fired Events</em>' reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getActionSignature_FiredEvents()
   * @model
   * @generated
   */
  EList<Event> getFiredEvents();

} // ActionSignature
