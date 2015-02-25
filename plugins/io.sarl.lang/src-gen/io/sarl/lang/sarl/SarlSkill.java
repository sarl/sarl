/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Skill</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.SarlSkill#getExtends <em>Extends</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.SarlSkill#getImplements <em>Implements</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill()
 * @model
 * @generated
 */
public interface SarlSkill extends XtendTypeDeclaration
{
  /**
   * Returns the value of the '<em><b>Extends</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmParameterizedTypeReference}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Extends</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Extends</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill_Extends()
   * @model containment="true"
   * @generated
   */
  EList<JvmParameterizedTypeReference> getExtends();

  /**
   * Returns the value of the '<em><b>Implements</b></em>' containment reference list.
   * The list contents are of type {@link org.eclipse.xtext.common.types.JvmParameterizedTypeReference}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Implements</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Implements</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSarlSkill_Implements()
   * @model containment="true"
   * @generated
   */
  EList<JvmParameterizedTypeReference> getImplements();

} // SarlSkill
