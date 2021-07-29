using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;
using KDaili.WaterSystem.Data;

namespace KDaili.WaterSystem
{
    [CustomEditor(typeof(WaterSystem))]
    public class WaterSystemEditor : Editor
    {
        private const string Title_Label = "<color=#87ceeb>Water Settings</color>";

        GUIStyle Title_LabelStyle => new GUIStyle()
        {
            fontSize = 30,
            richText = true,
            fontStyle = FontStyle.Bold,
        };

        public override void OnInspectorGUI()
        {
            EditorGUI.BeginChangeCheck();

            serializedObject.Update();
            var _target = (WaterSystem)target;

            EditorGUILayout.LabelField(Title_Label, Title_LabelStyle, GUILayout.Height(40));
            EditorGUILayout.Space(1);

            var isDevelop = serializedObject.FindProperty("develop");
            EditorGUILayout.PropertyField(isDevelop, true);

            if (!_target.develop)
            {
                using (new GUILayout.VerticalScope(GUI.skin.box))
                {
                    var waterColor = serializedObject.FindProperty("waterParameter");
                    EditorGUILayout.PropertyField(waterColor, true);
                    EditorGUI.indentLevel++;
                    if (waterColor.objectReferenceValue != null)
                    {
                        CreateEditor((WaterParameter)waterColor.objectReferenceValue).OnInspectorGUI();
                    }
                    EditorGUI.indentLevel--;
                }

                GUILayout.Space(5);

                using (new GUILayout.HorizontalScope())
                {
                    if (GUILayout.Button("Writeing Gradient Texture", GUILayout.Height(25)))
                    {
                        var filePath = EditorUtility.SaveFilePanel("Save", "Assets", "name", "png");
                        if (!string.IsNullOrEmpty(filePath))
                        {
                            _target.WriteGradientTexture(filePath);
                        }
                    }
                }
            }

            serializedObject.ApplyModifiedProperties();

            if (EditorGUI.EndChangeCheck())
            {
                UnityEditor.SceneManagement.EditorSceneManager.MarkAllScenesDirty();
                _target.Init();
            }
        }
    }
}