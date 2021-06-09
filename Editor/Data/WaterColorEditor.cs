using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEditor.Experimental.TerrainAPI;
using UnityEngine;

namespace KDaili.WaterSystem.Data
{
    [CustomEditor(typeof(WaterParameter))]
    public class WaterColorEditor : Editor
    {
        public override void OnInspectorGUI()
        {
            serializedObject.Update();

            // GradientColor
            var gradient = serializedObject.FindProperty("_WaterGradientColor");
            EditorGUILayout.PropertyField(gradient, new GUIContent("WaterColor", gradientTT), true, null);
            // NormalScale
            var normalScale = serializedObject.FindProperty("m_NormalScale");
            EditorGUILayout.Slider(normalScale, 0, 1, new GUIContent("Normal Scale", normalScaleTT));
            // NormalTiling
            var normalTiling = serializedObject.FindProperty("m_NormalTiling");
            EditorGUILayout.PropertyField(normalTiling, new GUIContent("Normal Tiling", normalTilingTT), true, null);
            // NormalScrollSpeed
            var normalScrollSpeed = serializedObject.FindProperty("m_NormalScrollSpeed");
            EditorGUILayout.PropertyField(normalScrollSpeed, new GUIContent("Normal Scroll Speed", normalScrollSpeedTT), true, null);
            // DistortionPower
            var distortionPower = serializedObject.FindProperty("m_DistortionPower");
            EditorGUILayout.PropertyField(distortionPower, new GUIContent("Distortion Power", distortionPowerTT), true, null);
            // WaterColorBlend
            var waterColorBlend = serializedObject.FindProperty("m_WaterColorBlend");
            EditorGUILayout.Slider(waterColorBlend, 0, 1, new GUIContent("Color Blend", waterColorBlendTT));
            // Smoothness
            var smoothness = serializedObject.FindProperty("m_Smoothness");
            EditorGUILayout.Slider(smoothness, 0, 1, new GUIContent("Smoothness"));
            // DepthDistance
            var depthDistance = serializedObject.FindProperty("m_DepthDistance");
            EditorGUILayout.PropertyField(depthDistance, new GUIContent("Depth Distance", distortionPowerTT), true, null);

            EditorUtility.SetDirty(this);
            serializedObject.ApplyModifiedProperties();
        }

        ///TOOLTIPS///
        private string gradientTT = "深度情報をもとに0～1で反映されます";
        private readonly string normalScaleTT = "ノーマルテクスチャサイズ";
        private readonly string normalTilingTT = "ノーマルテクスチャタイリング";
        private readonly string normalScrollSpeedTT = "深度情報をもとに0～1で反映されます";
        private readonly string distortionPowerTT = "深度情報をもとに0～1で反映されます";
        private readonly string waterColorBlendTT = "深度情報をもとに0～1で反映されます";
    }
}
